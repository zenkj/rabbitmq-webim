%%%-----------------------------------------------------------------------
%%% @author zenkj <juzejian@gmail.com>
%%% @copyright 2013-2015 zenkj
%%% @doc webim mqclient process
%%% @end
%%%-----------------------------------------------------------------------
-module(rabbit_webim_mqclient).

-behaviour(gen_server).

%% Client API
-export([
        start_link/3,  %% start
        subscribe/3,   %% web process subscribe for incoming messages
        unsubscribe/1,  %% web process stop or timeout, so unsubscribe
        stop/2,        %% stop, with system message
        stop/1,        %% stop, without message
        ack/1          %% acknowledge the messages received till now
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
        code_change/3]).

-include_lib("amqp_client/include/amqp_client.hrl").
-include("../include/rabbit_webim.hrl").
-include("../include/rabbit_webim_db.hrl").

-record(state, {channel,   %% mq channel
                sessionid, %% session id got from http cookie
                userid,    %% user id
                qname,     %% queue name of the specified user
                ctag,      %% consumer tag
                dtag,      %% delivery tag, used for acknowledge
                mq,        %% queue of received messages
                clientpid, %% client process id, used for sending messages
                friends,   %% friend list, including friend id and status
                fchanged = false, %% friends status has changed
                tref       %% timer reference
               }).

-define(SYSUSER, 0).
-define(TIMEOUT, 90000). 
-define(MAXMSG, 100).

%%%=======================================================================
%%% Client API
%%%=======================================================================

%%------------------------------------------------------------------------
%% @doc Starts the server.
%%
%% @spec start_link() -> {ok, Pid}
%% where
%%   Pid = pid()
%% @end
%%------------------------------------------------------------------------
start_link(SessionId, UserId, Channel) ->
    gen_server:start_link(?MODULE, {SessionId, UserId, Channel}, []).


%%------------------------------------------------------------------------
%% @doc subscribe.
%%
%% @spec subscribe(SessionId, UserId) -> ok | {error, Reason}
%% where
%%   SessionId = string()
%%   UserId = integer()
%% @end
%%------------------------------------------------------------------------
subscribe(CPid, SessionId, UserId) ->
    gen_server:call(CPid, {subscribe, SessionId, UserId}).

%%------------------------------------------------------------------------
%% @doc unsubscribe.
%%
%% @spec unsubscribe() -> ok
%% @end
%%------------------------------------------------------------------------
unsubscribe(CPid) ->
    gen_server:cast(CPid, {unsubscribe, self()}),
    ok.

%%------------------------------------------------------------------------
%% @doc stop.
%%
%% @spec stop(SystemMsg) -> ok
%% @end
%%------------------------------------------------------------------------
stop(CPid, SystemMsg) ->
    gen_server:cast(CPid, {stop, SystemMsg}),
    ok.

stop(CPid) -> stop(CPid, normal).

%%------------------------------------------------------------------------
%% @doc acknowledge the messages received till now, cause the mq remove
%%      these messages.
%%
%% @spec stop(SystemMsg) -> ok
%% @end
%%------------------------------------------------------------------------
ack(CPid) ->
    gen_server:cast(CPid, ack),
    ok.

%%%=======================================================================
%%% gen_server callbacks
%%%=======================================================================

init({SessionId, UserId, Channel}) ->
    Q = list_to_binary("q_" ++ UserId),
    COK = amqp_channel:call(Channel, #'basic.consume'{queue=Q}),
    #'basic.consume_ok'{consumer_tag = Tag} = COK,
    Fs = rabbit_webim_misc:friends_status(UserId),
    notify(Fs, UserId, come),
    {ok, Tref} = timer:send_after(?TIMEOUT, timeout),
    log("Set Timer in init: ~p~n", [Tref]),
    {ok, #state{channel   = Channel,
                sessionid = SessionId,
                userid    = UserId,
                qname     = Q,
                ctag      = Tag,
                dtag      = undefined,
                mq        = queue:new(),
                clientpid = undefined,
                friends   = Fs,
                fchanged  = false,
                tref      = Tref}}.

notify([], _UserId, _Info) ->
    ok;
notify([{#wim_user{id=Fid}, _Status} | T], UserId, Info) ->
    case rabbit_webim_session:get_session_of_user(Fid) of
        undefined -> ok;
        {_SessionId, Fid, MQCPid, _Channel} ->
            friend_notify(MQCPid, UserId, Info)
    end,
    notify(T, UserId, Info).

friend_notify(MQCPid, UserId, Info) ->
    gen_server:cast(MQCPid, {UserId, Info}).
    
    

reset_timer(State = #state{tref=Tref}) ->
    timer:cancel(Tref),
    log("cancel timer: ~p~n", [Tref]),
    Tref1 = timer:send_after(?TIMEOUT, timeout),
    log("reset timer: ~p~n", [Tref1]),
    State#state{tref=Tref1}.
stop_timer(State = #state{tref=Tref}) ->
    timer:cancel(Tref),
    log("cancel timer: ~p~n", [Tref]),
    State#state{tref=undefined}.

handle_call({subscribe, SessionId, UserId},
            {ClientPid, _Tag},
            State = #state{sessionid = SID,
                           userid    = UID}) ->
    log("handle subscribe~n"),
    State1 = reset_timer(State),
    EMsg = case {SessionId, UserId, ClientPid} of
                {SID, UID, _} when is_pid(ClientPid) ->
                    undefined;
                {SID, UID, _} ->
                    "invalid client process id";
                {_, UID, _} ->
                    "invalid session id";
                {SID, _, _} ->
                    "invalid user id"
           end,
    case EMsg of
        undefined ->
            State2 = State1#state{clientpid=ClientPid},
            State3 = maybe_send_msg(State2),
            {reply, ok, State3};
        _         ->
            {reply, {error, EMsg}, State1}
    end.

maybe_send_msg(State = #state{clientpid = undefined}) ->
    State;
maybe_send_msg(State = #state{clientpid = CPID, friends=Fs, fchanged=true}) when is_pid(CPID)->
    FS = [{Uid, Status} || {#wim_user{id=Uid}, Status} <- Fs],
    CPID ! {friend_status_change, FS},
    %%io:format("send status change message: ~p~n", [FS]),
    State#state{fchanged=false};
maybe_send_msg(State = #state{mq = MQ, clientpid = CPID}) when is_pid(CPID)->
    case queue:len(MQ) of
        0 -> State;
        N when N < ?MAXMSG ->
            send_msg(MQ, CPID),
            State#state{mq=queue:new(), clientpid=undefined};
        _N ->
            {MQ1, MQ2} = queue:split(?MAXMSG, MQ),
            send_msg(MQ1, CPID),
            State#state{mq=MQ2, clientpid=undefined}
    end.
send_msg(MQ, CPID) ->
    Msgs = queue:to_list(MQ),
    CPID ! {deliver_msg, Msgs}.


handle_cast({stop, SystemMsg}, State = #state{userid=UserId, mq=MQ}) ->
    State1 = stop_timer(State),
    State2 = case SystemMsg of
                normal -> State1;
                Info when is_list(Info) ->
                    Msg = #msg{from=?SYSUSER, to=UserId, time=rabbit_webim_misc:mkbintime(),  content=list_to_binary(Info)},
                    State1#state{mq = queue:in(Msg, MQ)}
             end,
    State3 = maybe_send_msg(State2),
    {stop, normal, State3};
handle_cast(ack, State = #state{dtag = undefined}) ->
    {noreply, State};
handle_cast(ack, State = #state{channel = Channel, dtag = DTag}) ->
    amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = DTag, multiple = true}),
    {noreply, State#state{dtag = undefined}};
handle_cast({unsubscribe, ClientPid},
            State = #state{clientpid = CPID}) ->
    case ClientPid of
        CPID -> {noreply, State#state{clientpid = undefined}};
        _    -> {noreply, State}
    end;
handle_cast({Fid, come}, State=#state{friends=Fs}) ->
    Fs1 = [case Uid of
            Fid -> {U, active};
            _   -> {U, Status}
           end || {U=#wim_user{id=Uid}, Status} <- Fs],
    State1 = maybe_send_msg(State#state{friends=Fs1, fchanged=true}),
    {noreply, State1};
handle_cast({Fid, leave}, State=#state{friends=Fs}) ->
    Fs1 = [case Uid of
            Fid -> {U, inactive};
            _   -> {U, Status}
           end || {U=#wim_user{id=Uid}, Status} <- Fs],
    State1 = maybe_send_msg(State#state{friends=Fs1, fchanged=true}),
    {noreply, State1}.

handle_info(timeout, State = #state{userid=_UserId}) ->
    %%rabbit_webim_webserver:leave(UserId, "time out, quit"),
    {noreply, State};
handle_info(#'basic.consume_ok'{}, State) ->
    {noreply, State};
handle_info(#'basic.cancel_ok'{}, State) ->
    {noreply, State};
handle_info({#'basic.deliver'{delivery_tag = Tag},
             _Content = #amqp_msg{props=#'P_basic'{headers=Headers}, payload=Payload}},
            State = #state{mq = MQ}) ->
    {longstr, FromUID} = rabbit_misc:table_lookup(Headers, <<"wim-from-user-id">>),
    {longstr, ToUID} = rabbit_misc:table_lookup(Headers, <<"wim-to-user-id">>),
    {longstr, Time} = rabbit_misc:table_lookup(Headers, <<"wim-timestamp">>),
    MQ1 = queue:in(#msg{from=FromUID, to=ToUID, time=Time, content=Payload}, MQ),
    State1 = maybe_send_msg(State#state{dtag=Tag, mq=MQ1}),
    {noreply, State1}.
    

terminate(_Reason, _State = #state{channel = Channel, ctag = Tag, userid = UserId, friends=Fs}) ->
    amqp_channel:call(Channel, #'basic.cancel'{consumer_tag = Tag}),
    notify(Fs, UserId, leave),
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.


log(Msg) ->
    io:format(Msg).
log(Msg, Data) ->
    io:format(Msg, Data).
