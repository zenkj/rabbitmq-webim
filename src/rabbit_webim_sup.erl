%% Copyright (c) 2013 Zenk J.
%% You may use this code for any purpose.

-module(rabbit_webim_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = []).

init([]) ->
    {ok, {{one_for_one, 3, 10},
          [{rabbit_webim_session,
            {rabbit_webim_session, start_link, []},
            permanent,
            10000,
            worker,
            [rabbit_webim_session]},
           {rabbit_webim_webserver,
            {rabbit_webim_webserver, start_link, []},
            permanent,
            10000,
            worker,
            [rabbit_webim_webserver]},
           {rabbit_webim_mqclient_sup,
            {rabbit_webim_mqclient_sup, start_link, []},
            permanent,
            10000,
            supervisor,
            [rabbit_webim_mqclient_sup]}
          ]}}.
