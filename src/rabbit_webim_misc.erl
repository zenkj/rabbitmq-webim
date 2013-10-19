-module(rabbit_webim_misc).

-export([mktime/0, mkbintime/0, friends_status/1]).

-include("../include/rabbit_webim_db.hrl").

mktime() ->
    {Y,M,D} = erlang:date(),
    {H,Mn,S} = erlang:time(),
    io_lib:format("~p/~p/~p ~p:~p:~p", [Y,M,D,H,Mn,S]).

mkbintime() ->
    list_to_binary(mktime()).

friends_status(UserId) ->
    Fs = rabbit_webim_db:get_friends(UserId),
    [{F, state(Fid)} || F=#wim_user{id=Fid} <- Fs].

state(Uid) ->
    case rabbit_webim_session:get_session_of_user(Uid) of
        undefined -> inactive;
        _         -> active
    end.

