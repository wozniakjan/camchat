-module(camchat_sockjs).
-export([camchat_sockjs/3]).



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

camchat_sockjs(Conn, {recv, Data}, init) ->
    <<"connect:", Room/bitstring>> = Data,
    {ok, Type} = rooms:connect(Room, Conn, []),
    Conn:send(lists:append("connected:", atom_to_list(Type))),
    send_peers(Conn, Room, <<"peer_connected">>),
    {ok, connected};
camchat_sockjs(Conn, {recv, Data}, connected) ->
    send_peers(Conn, Data),
    {ok, connected}.


gracefully_close(Conn) ->
    Peers = rooms:disconnect(Conn, []),
    lists:map(fun(Peer)-> Peer:send(<<"peer_disconnected">>) end, Peers), ok.

send_peers(Conn, Msg) ->
    Peers = rooms:get_peers(Conn),
    lists:map(fun(Peer)-> Peer:send(Msg) end, Peers).
send_peers(Conn, Room, Msg) ->
    Peers = rooms:get_peers(Conn, Room),
    lists:map(fun(Peer)-> Peer:send(Msg) end, Peers).
