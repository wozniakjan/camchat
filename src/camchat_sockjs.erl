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

parse_msg(_Conn, {[{<<"connect">>, _Room}]}, init) ->
    {ok, waiting_for_media};
parse_msg(Conn, {[{<<"ready">>, Room}]}, waiting_for_media) ->
    {ok, RoomStatus, User} = rooms:connect(Room, Conn, []),
    UserId = User#user.user_id,
    UN = User#user.username,
    PeerList = send_peers(Conn, jiffy:encode({[{peer_connected, UserId}, {name, UN}]})),
    UsernameList = lists:map(fun(X)-> {X#user.user_id, X#user.username} end, PeerList),
    Conn:send(jiffy:encode({[{connected, RoomStatus}, {user_id, UserId}, {user_name, UN}, {peer_list, {UsernameList}}]})),
    {ok, connected};
parse_msg(Conn, {[{<<"ready">>, Room}, {<<"user_name">>, UN}]}, waiting_for_media) ->
    {ok, RoomStatus, User} = rooms:connect(Room, Conn, []),
    UserId = User#user.user_id,
    rooms:edit_user(Conn, ?USERNAME_POS, UN),
    PeerList = send_peers(Conn, jiffy:encode({[{peer_connected, UserId}, {name, UN}]})),
    UsernameList = lists:map(fun(X)-> {X#user.user_id, X#user.username} end, PeerList),
    Conn:send(jiffy:encode({[{connected, RoomStatus}, {user_id, UserId}, {user_name, UN}, {peer_list, {UsernameList}}]})),
    {ok, connected};
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
    rooms:edit_user(Conn, ?USERNAME_POS, NewName),
    Id = rooms:get_user_id_by_conn(Conn),
    send_peers(Conn, jiffy:encode({[{<<"change_name">>, NewName}, {<<"id">>, Id}]})),
    {ok, connected};
parse_msg(Conn, {[{<<"audio_energy">>, Energy}]}, connected) ->
    Id = rooms:get_user_id_by_conn(Conn),
    send_peers(Conn, jiffy:encode({[{<<"audio_energy">>, Energy}, {<<"id">>, Id}]})),
    {ok, connected};
parse_msg(Conn, {[{<<"select_stream">>, Stream}]}, connected) ->
    Id = rooms:get_user_id_by_conn(Conn),
    send_peers(Conn, jiffy:encode({[{<<"select_stream">>, Stream}, {<<"id">>, Id}]})),
    {ok, connected}.

gracefully_close(Conn) ->
    {User, Peers} = rooms:disconnect(Conn),
    Reply = jiffy:encode({[{peer_disconnected, User#user.user_id}]}),
    lists:map(fun(Peer)-> Peer:send(Reply) end, Peers), ok.

send_peers(Conn, Msg) ->
    Peers = rooms:get_peers(Conn),
    lists:map(fun(Peer)-> (Peer#user.connection_id):send(Msg) end, Peers),
    Peers.
