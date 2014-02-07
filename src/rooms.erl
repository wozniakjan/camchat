-module(rooms).
-export([start/0, connect/3, disconnect/3, disconnect/2, get_peers/1, get_peers/2, stop/0]).

-record(room, {room_id, user_list}).
-define(ROOM_ID_POS, 2).
-define(USER_LIST_POS, 3).
-record(user, {connection_id, room_id, user_id, username}).
-define(CONNECTION_ID_POS, 2).

start() ->
    ets:new(rooms, [set, named_table, public, {keypos, ?ROOM_ID_POS}]),
    ets:new(users, [set, named_table, public, {keypos, ?CONNECTION_ID_POS}]).

create_user(ConnectionId, RoomId) ->
    UID = "uid",
    UN = "User Name",
    #user{connection_id=ConnectionId, room_id=RoomId, user_id=UID, username=UN}.

connect(RoomId, ConnectionId, Opt) ->
    case ets:lookup(rooms, RoomId) of
        [] -> 
            Room = #room{room_id=RoomId, user_list=[ConnectionId]},
            ets:insert(rooms, Room),
            ets:insert(users, create_user(ConnectionId, RoomId)),
            {ok, new};
        [ExistingRoom] ->
            RoomId = ExistingRoom#room.room_id,
            UserList = ExistingRoom#room.user_list,
            ets:update_element(rooms, RoomId, {?USER_LIST_POS, [ConnectionId | UserList]}),
            ets:insert(users, create_user(ConnectionId, RoomId)),
            {ok, existing}
    end.
            
disconnect(ConnectionId, Opt) ->
    [User] = ets:lookup(users, ConnectionId),
    disconnect(User#user.room_id, ConnectionId, Opt).
    
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
    lists:delete(ConnectionId, Room#room.user_list).

stop() ->
    ets:delete(rooms).
