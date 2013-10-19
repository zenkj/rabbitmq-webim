%%%-----------------------------------------------------------------------
%%% @author Zenk J. <juzejian@gmail.com>
%%% @copyright 2013-2015 Zenk J.
%%% @doc webim's mqclient process supervisor
%%% @end
%%%-----------------------------------------------------------------------
-module(rabbit_webim_mqclient_sup).

-behaviour(supervisor).

%% Client API
-export([start_link/0, start_mqclient/3]).

%% supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%=======================================================================
%%% Client API
%%%=======================================================================

%%------------------------------------------------------------------------
%% @doc Starts the supervisor.
%%
%% @spec start_link() -> {ok, Pid}
%% where
%%   Pid = pid()
%% @end
%%------------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_mqclient(SessionId, UserId, Channel) ->
    supervisor:start_child(?SERVER, [SessionId, UserId, Channel]).

%%%=======================================================================
%%% supervisor callbacks
%%%=======================================================================

init([]) ->
    Server1 = {webim_mqclient, {rabbit_webim_mqclient, start_link, []},
                transient, 10000, worker, [rabbit_webim_mqclient]},
    Children = [Server1],
    RestartCount = 4,
    WithinSeconds = 100,
    RestartStrategy = {simple_one_for_one, RestartCount, WithinSeconds},
    {ok, {RestartStrategy, Children}}.

