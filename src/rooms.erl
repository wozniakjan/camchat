-module(rooms).
-export([start/0, connect/3, disconnect/2, get_peers/1, get_peers/2, stop/0, reload/1]).

-include("types.hrl").

start() ->
    ets:new(rooms, [ordered_set, named_table, public, {keypos, ?ROOM_ID_POS}]),
    ets:new(users, [ordered_set, named_table, public, {keypos, ?CONNECTION_ID_POS}]).

create_id() ->
    {_, T} = lists:split(5, erlang:ref_to_list(make_ref())),
    IdT = lists:delete(62, T), 
    Id = re:replace(IdT,"\\.","_",[{return, list}, global]),
    erlang:list_to_bitstring(Id).

create_user(ConnectionId, RoomId) ->
    UID = create_id(),
    UN = <<"User Name">>,
    #user{connection_id=ConnectionId, room_id=RoomId, user_id=UID, username=UN}.

connect(RoomId, ConnectionId, _Opt) ->
    RoomStatus = case ets:lookup(rooms, RoomId) of
        [] -> 
            Room = #room{room_id=RoomId, user_list=[ConnectionId]},
            ets:insert(rooms, Room),
            new_room;
        [ExistingRoom] ->
            RoomId = ExistingRoom#room.room_id,
            UserList = ExistingRoom#room.user_list,
            ets:update_element(rooms, RoomId, {?USER_LIST_POS, [ConnectionId | UserList]}),
            existing_room
    end,
    User = create_user(ConnectionId, RoomId),
    ets:insert(users, User),
    {ok, RoomStatus, User}.
        
            
disconnect(ConnectionId, Opt) ->
    [User] = ets:lookup(users, ConnectionId),
    {User, disconnect(User#user.room_id, ConnectionId, Opt)}.
    
disconnect(RoomId, ConnectionId, _Opt) ->
    [Room] = ets:lookup(rooms, RoomId),
    case lists:delete(ConnectionId, Room#room.user_list) of
        [] -> 
            ets:delete(rooms, RoomId),
            ets:delete(users, ConnectionId),
            [];
        UpdatedList ->
            ets:update_element(rooms, RoomId, {?USER_LIST_POS, UpdatedList}),
            ets:delete(users, ConnectionId),
            UpdatedList
    end.

get_peers(ConnectionId) ->
    [User] = ets:lookup(users, ConnectionId),
    get_peers(ConnectionId, User#user.room_id).

get_peers(ConnectionId, RoomId) ->
    [Room] = ets:lookup(rooms, RoomId),
    PeersList = lists:delete(ConnectionId, Room#room.user_list),
    lists:map(fun(PeerConId)-> [X]=ets:lookup(users, PeerConId), X end, PeersList). 

stop() ->
    ets:delete(rooms).

reload(_Opts) -> reloaded.
