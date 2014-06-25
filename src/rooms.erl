-module(rooms).
-export([start/0, connect/3, disconnect/1, get_peers/1, get_peers/2, stop/0, reload/1, 
         edit_user/3]).
-export([get_conn_by_user_id/1, get_user_id_by_conn/1, get_room_default_stream/1]).

-include("types.hrl").

start() ->
    ets:new(rooms, [ordered_set, named_table, public, {keypos, ?ROOM_ID_POS}]),
    ets:new(uid_lookup, [ordered_set, named_table, public]),
    ets:new(users, [ordered_set, named_table, public, {keypos, ?CONNECTION_ID_POS}]).

create_id() ->
    %TODO: check for global uid collision
    {_, T} = lists:split(5, erlang:ref_to_list(make_ref())),
    IdT = lists:delete(62, T), 
    Id = re:replace(IdT,"\\.","_",[{return, list}, global]),
    erlang:list_to_bitstring(Id).

create_user(ConnectionId, RoomId) ->
    UID = create_id(),
    UN = name_generator:get_name(),
    #user{connection_id=ConnectionId, room_id=RoomId, user_id=UID, username=UN}.

get_room_default_stream(Room) ->
    case ets:lookup(rooms, Room) of
        [] -> <<"default">>;
        [ExistingRoom] -> ExistingRoom#room.default_stream
    end.

connect(RoomId, ConnectionId, Params) ->
    RoomStatus = case ets:lookup(rooms, RoomId) of
        [] -> 
            Room = case lists:keyfind(<<"default_stream">>, 1, Params) of
                false -> #room{room_id=RoomId, user_list=[ConnectionId], 
                        default_stream= <<"camera">>};
                {_, S} -> #room{room_id=RoomId, user_list=[ConnectionId], 
                        default_stream=S}
            end,
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
    ets:insert(uid_lookup, {User#user.user_id, User#user.connection_id}),
    {ok, RoomStatus, User}.
        
            
disconnect(ConnectionId) ->
    [User] = ets:lookup(users, ConnectionId),
    {User, disconnect(User#user.room_id, ConnectionId)}.
    
disconnect(RoomId, ConnectionId) ->
    [Room] = ets:lookup(rooms, RoomId),
    UpdatedList = lists:delete(ConnectionId, Room#room.user_list),
    case UpdatedList of
        [] -> 
            ets:delete(rooms, RoomId);
        _ ->
            ets:update_element(rooms, RoomId, {?USER_LIST_POS, UpdatedList})
    end,
    [User] = ets:lookup(users, ConnectionId),
    ets:delete(users, ConnectionId),
    ets:delete(uid_lookup, User#user.user_id),
    UpdatedList.

get_peers(ConnectionId) ->
    [User] = ets:lookup(users, ConnectionId),
    get_peers(ConnectionId, User#user.room_id).

get_peers(ConnectionId, RoomId) ->
    [Room] = ets:lookup(rooms, RoomId),
    PeersList = lists:delete(ConnectionId, Room#room.user_list),
    lists:map(fun(PeerConId)-> [X]=ets:lookup(users, PeerConId), X end, PeersList). 

get_conn_by_user_id(UserId) ->
    [{UserId, ConnectionId}] = ets:lookup(uid_lookup, UserId),
    ConnectionId.

get_user_id_by_conn(Conn) ->
    [User] = ets:lookup(users, Conn),
    User#user.user_id.    

stop() ->
    ets:delete(rooms).

edit_user(ConnectionId, Attr, Val) ->
    ets:update_element(users, ConnectionId, {Attr, Val}).

reload(_Opts) -> reloaded.
