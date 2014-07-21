-module(rooms).
-export([start/0, connect/3, disconnect/1, get_peers/1, get_peers/2, stop/0,  
         edit_user/3]).
-export([get_conn_by_user_id/1, get_user_id_by_conn/1, get_room_default_stream/1]).
-export([get_random/0, get_empty/0]).
-export([room_update/2]).

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

create_user(ConnectionId, RoomId, Params) ->
    UN = name_generator:get_name(),
    UID = create_id(),
    lists:foldl(
        fun(Param, User) -> 
            case Param of
                {<<"browser_token">>, P} -> User#user{browser_token = P};
                {<<"user_name">>, P} -> User#user{username = P};
                _ -> User  % unknown user parameter
            end
        end,
        #user{connection_id=ConnectionId, room_id=RoomId, user_id=UID, username=UN},
        Params).

get_room_default_stream(Room) ->
    case ets:lookup(rooms, Room) of
        [] -> <<"default">>;
        [ExistingRoom] -> ExistingRoom#room.default_stream
    end.

parse_room_params(RoomId, ConnectionId, Params) ->
    lists:foldl(
        fun(Param, Room) -> 
            case Param of
                {<<"default_stream">>, P} -> Room#room{default_stream = P};
                {<<"key">>, P} -> Room#room{key = P};
                _ -> Room  % unknown room parameter
            end
        end,
        #room{room_id=RoomId, user_list=[ConnectionId]},
        Params).

room_update(ConnectionId, Params) ->
    [User] = ets:lookup(users, ConnectionId),
    Room = User#user.room_id,
    lists:map(
        fun(Param) -> 
            case Param of
                {<<"default_stream">>, P} -> 
                    ets:update_element(rooms, Room, {?DEFAULT_STREAM_POS, P});
                {<<"key">>, P} -> 
                    ets:update_element(rooms, Room, {?KEY_POS, P});
                _ -> ok  % unknown room parameter
            end
        end,
        Params).

match_key(#room{key = Pwd}, _) when Pwd == <<"">> -> ok;
match_key(#room{key = Pwd}, Params) ->
    case lists:keyfind(<<"key">>, 1, Params) of
        {_, Pwd} -> ok;
        _ -> throw({error, <<"wrong_key">>})
    end.

connect(RoomId, ConnectionId, Params) ->
    RoomStatus = case ets:lookup(rooms, RoomId) of
        [] -> 
            Room = parse_room_params(RoomId, ConnectionId, Params),
            ets:insert(rooms, Room),
            new_room;
        [ExistingRoom] ->
            match_key(ExistingRoom, Params),
            RoomId = ExistingRoom#room.room_id,
            UserList = ExistingRoom#room.user_list,
            ets:update_element(rooms, RoomId, {?USER_LIST_POS, [ConnectionId | UserList]}),
            existing_room
    end,
    User = create_user(ConnectionId, RoomId, Params),
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

%TODO: check for collisions
get_empty() -> 
    name_generator:get_room().

get_nth(N, Prev) when N =< 1 -> Prev;
get_nth(N, Prev) -> get_nth(N-1, ets:next(rooms, Prev)).

get_random_helper() ->
    random:seed(now()),
    Index = random:uniform(ets:info(rooms, size)),
    get_nth(Index, ets:first(rooms)).

get_random() ->
    try get_random_helper() of
        Room -> Room
    catch
        E:X -> 
            lager:info("room:get_random() ~p:~p",[E,X]),
            get_empty()
    end.
