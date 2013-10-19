-module(rabbit_webim_session).

-behavior(gen_server).

-export([start_link/0, get_session/1, get_session_of_user/1, add_session/4, del_session/1, del_session_of_user/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_session_of_user(UserId) ->
    gen_server:call(?SERVER, {get_session_of_user, UserId}).

get_session(SessionId) ->
    gen_server:call(?SERVER, {get_session, SessionId}).
add_session(SessionId, UserId, MQClientPid, MQChannel) ->
    gen_server:call(?SERVER, {add_session, SessionId, UserId, MQClientPid, MQChannel}).
del_session(SessionId) ->
    gen_server:call(?SERVER, {del_session, SessionId}).
del_session_of_user(UserId) ->
    gen_server:call(?SERVER, {del_session_of_user, UserId}).



init([]) ->
    {ok, []}.

handle_call({get_session_of_user, UserId}, _From, State) ->
    {reply, get({session, UserId}), State};
handle_call({get_session, SessionId}, _From, State) ->
    case get({userid, SessionId}) of
        undefined -> {reply, undefined, State};
        UserId    -> {reply, get({session, UserId}), State}
    end;
handle_call({del_session, SessionId}, _From, State) ->
    case erase({userid, SessionId}) of
        undefined -> {reply, undefined, State};
        UserId    -> {reply, erase({session, UserId}), State}
    end;
handle_call({del_session_of_user, UserId}, _From, State) ->
    case erase({session, UserId}) of
        undefined -> {reply, undefined, State};
        Session = {SessionId, UserId, _, _}    ->
                erase({userid, SessionId}),
                {reply, Session, State}
    end;
handle_call({add_session, SessionId, UserId, MQClientPid, MQChannel}, _From, State) ->
    undefined = get({userid, SessionId}),
    put({session, UserId}, {SessionId, UserId, MQClientPid, MQChannel}),
    put({userid, SessionId}, UserId),
    {reply, ok, State}.

handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
