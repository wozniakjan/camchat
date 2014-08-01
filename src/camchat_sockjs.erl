-module(camchat_sockjs).
-export([camchat_sockjs/3]).

-include("types.hrl").

camchat_sockjs(_Conn, init, init) -> 
    {ok, init};
camchat_sockjs(_Conn, {info, _Info}, State) -> 
    {ok, State};
camchat_sockjs(Conn, closed, _State) -> 
    try gracefully_close(Conn) of
        ok -> {ok, killed}
    catch
        TypeOfError:Exception ->
            lager:error("Disconnection ~p: ~p",[TypeOfError, Exception])
    end;
camchat_sockjs(Conn, {recv, Data}, State) ->
    Msg = jiffy:decode(Data),
    try_parse_msg(Conn, Msg, State).

try_parse_msg(Conn, Msg, Status) ->
    try parse_msg(Conn, Msg, Status) of
        {ok, Type} -> {ok, Type}
    catch
        TypeOfError:Exception ->
            lager:error("Unable to process msg: ~p - ~p: ~p",[Msg, TypeOfError, Exception])
    end.

parse_msg(Conn, {[{<<"connect">>, Room}]}, init) ->
    DefaultStream = rooms:get_room_default_stream(Room),
    Conn:send(jiffy:encode({[{<<"init_stream">>, DefaultStream}]})),
    {ok, waiting_for_params};
parse_msg(Conn, {[{<<"ready">>, Room} | Params]}, waiting_for_params) ->
    case rooms:connect(Room, Conn, Params) of
        {ok, RoomStatus, User} -> 
            connect_reply(User, RoomStatus),
            {ok, connected};
        {error, wrong_key, User} ->
            Conn:send(jiffy:encode({[{error, <<"wrong_key">>}]})),
            UserId = User#user.user_id,
            Username = User#user.username,
            broadcast(User#user.room_id, {[{knock, UserId}, {username, Username}]}),
            {ok, waiting_for_params};
        {error, Reason} ->
            Conn:send(jiffy:encode({[{error, Reason}]})),
            {ok, waiting_for_params}
    end;
parse_msg(_Conn, {[{<<"offer">>, Offer}, {<<"caller">>, Caller}, {<<"callee">>, Callee}]}, connected) ->
    CalleeConn = rooms:get_conn_by_user_id(Callee),
    CalleeConn:send(jiffy:encode({[{<<"offer">>, Offer}, {<<"caller">>, Caller}, {<<"callee">>, Callee}]})),
    {ok, connected};
parse_msg(_Conn, {[{<<"answer">>, Answer}, {<<"caller">>, Caller}, {<<"callee">>, Callee}]}, connected) ->
    CallerConn = rooms:get_conn_by_user_id(Caller),
    CallerConn:send(jiffy:encode({[{<<"answer">>, Answer}, {<<"caller">>, Caller}, {<<"callee">>, Callee}]})),
    {ok, connected};
parse_msg(_Conn, {[{<<"ice_candidate">>, IC}, {<<"caller">>, From}, {<<"callee">>, To}]}, connected) ->
    ToConn = rooms:get_conn_by_user_id(To),
    ToConn:send(jiffy:encode({[{<<"ice_candidate">>, IC}, {<<"caller">>, From}, {<<"callee">>, To}]})),
    {ok, connected};
parse_msg(Conn, {[{<<"change_name">>, NewName}]}, connected) ->
    rooms:edit_user(Conn, #user.username, NewName),
    Id = rooms:get_user_id_by_conn(Conn),
    send_peers(Conn, jiffy:encode({[{<<"change_name">>, NewName}, {<<"id">>, Id}]})),
    {ok, connected};
parse_msg(Conn, {[{<<"audio_energy">>, Energy}]}, connected) ->
    Id = rooms:get_user_id_by_conn(Conn),
    send_peers(Conn, jiffy:encode({[{<<"audio_energy">>, Energy}, {<<"id">>, Id}]})),
    {ok, connected};
parse_msg(Conn, {[{<<"room_update">>, Type} | Settings]}, connected) ->
    rooms:room_update(Conn, Settings),
    send_peers(Conn, jiffy:encode({[{<<"room_update">>, Type} | Settings]})),
    {ok, connected};
parse_msg(_Conn, {[{<<"let_in">>, KnockId}]}, connected) ->
    {ok, Key, KConn} = rooms:let_in(KnockId),
    KConn:send(jiffy:encode({[{<<"let_in">>, Key}]})),
    {ok, connected};
parse_msg(Conn, {[{<<"select_stream">>, Stream}, StreamType]}, connected) ->
    Id = rooms:get_user_id_by_conn(Conn),
    send_peers(Conn, jiffy:encode({[{<<"select_stream">>, Stream}, {<<"id">>, Id}, StreamType]})),
    {ok, connected}.

gracefully_close(Conn) ->
    {User, Peers} = rooms:disconnect(Conn),
    Reply = jiffy:encode({[{peer_disconnected, User#user.user_id}]}),
    lists:map(fun(Peer)-> Peer:send(Reply) end, Peers), ok.

broadcast(Room, Msg) ->
    lists:map(fun(C)-> (C#user.connection_id):send(jiffy:encode(Msg)) end, rooms:get_users(Room)).

connect_reply(User, RoomStatus) ->
    UserId = User#user.user_id,
    UN = User#user.username,
    Token = User#user.browser_token,
    Conn = User#user.connection_id,
    Browser = User#user.browser,
    PeerMsg = {[{peer_connected, UserId}, {name, UN}, 
                {browser_token, Token}, {browser, Browser}]},
    PeerList = send_peers(Conn, jiffy:encode(PeerMsg)),
    UserList = lists:map(fun(X)-> 
                {X#user.user_id, 
                    {[{user_name, X#user.username}, 
                      {browser_token, X#user.browser_token},
                      {browser, X#user.browser}]}} 
        end, PeerList),
    Reply = [{user_id, UserId}, {user_name, UN}, {peer_list, {UserList}}],
    Conn:send(jiffy:encode({[{connected, RoomStatus} | Reply]})).

send_peers(Conn, Msg) ->
    Peers = rooms:get_peers(Conn),
    lists:map(fun(Peer)-> (Peer#user.connection_id):send(Msg) end, Peers),
    Peers.
