-module(rabbit_webim_db).

-export([all_users/0, get_user/1, add_user/3, get_friends/1, add_friend/2, remove_friend/2, validate/2]).

-include("../include/rabbit_webim_db.hrl").
-include_lib("stdlib/include/qlc.hrl").

all_users() ->
    tx(fun()->mnesia:all_keys(rabbit_webim_user) end).
        
get_user(Uid) when is_list(Uid) ->
    mnesia:dirty_read(rabbit_webim_user, Uid).

add_user(Name, Password, Email) ->
    Id = next_uid(),
    Encrypted_Password = encrypt(Id, Password),
    tx(fun() -> add_user_in_tx(Id, Name, Encrypted_Password, Email) end).

get_friends(Uid) when is_list(Uid) ->
    Fs = mnesia:dirty_read(rabbit_webim_friend, Uid),
    [F || #wim_friend{fid=Fid} <- Fs, F <- get_user(Fid)].

add_friend(Uid, Fid) ->
    tx(fun() -> add_friend_in_tx(Uid, Fid) end).

remove_friend(Uid, Fid) ->
    tx(fun() -> remove_friend_in_tx(Uid, Fid) end).

validate(Uid, Password) ->
    EPassword = encrypt(Uid, Password),
    case get_user(Uid) of
        [] -> false;
        [#wim_user{id=Uid, password=EPassword}] -> true;
        _ -> false    
    end.

tx(Fun) ->
    {atomic, R} = mnesia:transaction(Fun),
    R.

add_user_in_tx(Id, Name, EPassword, Email) ->
    User = #wim_user{id=Id, name=Name, password=EPassword, email=Email},
    mnesia:write(rabbit_webim_user, User, write),
    User.

get_friends_in_tx(QUid) ->
    Fids = qlc:e(qlc:q([Fid || #wim_friend{uid=Uid, fid=Fid} <- mnesia:table(rabbit_webim_friend), QUid == Uid])),
    case Fids of
        [] -> [];
        [_|_] ->
            qlc:e(qlc:q([F || F=#wim_user{id=Id1} <- mnesia:table(rabbit_webim_user), Id2 <- Fids, Id1 == Id2]))
    end.

add_friend_in_tx(Uid, Fid) ->
    mnesia:write(rabbit_webim_friend, #wim_friend{uid=Uid, fid=Fid}, write).

remove_friend_in_tx(Uid, Fid) ->
    mnesia:delete_object(rabbit_webim_friend, #wim_friend{uid=Uid, fid=Fid}).

next_uid() ->
    integer_to_list(mnesia:dirty_update_counter(rabbit_webim_next_id, uid, 1)).   

encrypt(Uid, Password) ->
    erlang:md5([Uid, Password]).

