-module(rabbit_webim_mnesia).

-export([init/0]).

-include("../include/rabbit_webim_db.hrl").

init() ->
    ensure_mnesia_running(),
    create_table().

ensure_mnesia_running() ->
    case mnesia:system_info(is_running) of
        yes ->
            ok;
        starting ->
            wait_for(mnesia_running),
            ensure_mnesia_running();
        Reason when Reason =:= no; Reason =:= stopping ->
            throw({error, mnesia_not_running})
    end.

wait_for(Condition) ->
    error_logger:info_msg("Waiting for ~p...~n", [Condition]),
    timer:sleep(1000).

create_table() ->
    [ok = create_table(Def) || Def <- definitions()].

create_table({TName, Options}) when is_list(Options) ->
    case mnesia:create_table(TName, Options) of
        {atomic, ok} -> ok;
        {aborted, {already_exists, TName}} -> ok;
        {aborted, Reason} -> {error, Reason}
    end.

definitions() ->
    [{rabbit_webim_user,     [{disc_copies, [node()]},
                              {record_name, wim_user},
                              {attributes, record_info(fields, wim_user)}]},
     {rabbit_webim_friend,   [{disc_copies, [node()]},
                              {record_name, wim_friend},
                              {attributes, record_info(fields, wim_friend)},
                              {type, bag}]},
     {rabbit_webim_next_id,  [{disc_copies, [node()]},
                              {attributes, [key, value]}]}
    ].
