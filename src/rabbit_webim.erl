%% Copyright (c) 2013 Zenk J.
%% You may use this code for any purpose.

-module(rabbit_webim).

-behaviour(application).

-export([start/2, stop/1]).

start(normal, []) ->
    {ok, Listener} = application:get_env(rabbitmq_webim, listener),
    log_startup(Listener),
    rabbit_webim_mnesia:init(),
    {ok, Pid} = rabbit_webim_sup:start_link(),
    {ok, Pid, []}.

stop([]) ->
    ok.

%%setup_amqp() ->
%%    amqp_connection:start(#amqp_params_direct{}).
%%
%%stop_amqp(Connection) ->
%%    amqp_connection:close(Connection).


log_startup(Listener) ->
    rabbit_log:info("WebIM plugin started. Port: ~w~n", [port(Listener)]).

port(Listener) ->
    proplists:get_value(port, Listener).
