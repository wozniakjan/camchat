-module(rooms).
-export([start/0, connect/3, disconnect/3, disconnect/2, stop/0]).

-record(room, {room_id, user_list}).
-define(ROOM_ID_POS, 2).
-define(USER_LIST_POS, 3).

start() ->
    ets:new(rooms, [set, named_table, public, {keypos, ?ROOM_ID_POS}]),
    ets:new(users, [set, named_table, public]).

connect(RoomId, UserId, _Opt) ->
    case ets:lookup(rooms, RoomId) of
        [] -> 
            Room = #room{room_id=RoomId, user_list=[UserId]},
            ets:insert(rooms, Room),
            ets:insert(users, {UserId, RoomId}),
            {ok, new};
        [ExistingRoom] ->
            RoomId = ExistingRoom#room.room_id,
            UserList = ExistingRoom#room.user_list,
            ets:update_element(rooms, RoomId, {?USER_LIST_POS, [UserId | UserList]}),
            ets:insert(users, {UserId, RoomId}),
            {ok, existing}
    end.
            
disconnect(UserId, Opt) ->
    [{_User, RoomId}] = ets:lookup(users, UserId),
    disconnect(RoomId, UserId, Opt).
disconnect(RoomId, UserId, _Opt) ->
    [Room] = ets:lookup(rooms, RoomId),
    case lists:delete(UserId, Room#room.user_list) of
        [] -> 
            ets:delete(rooms, RoomId),
            ets:delete(users, UserId);
        UpdatedList ->
            ets:update_element(rooms, RoomId, {?USER_LIST_POS, UpdatedList}),
            ets:delete(users, UserId)
    end.

stop() ->
    ets:delete(rooms).
