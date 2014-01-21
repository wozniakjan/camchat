-module(camchat_sockjs).
-export([camchat_sockjs/3]).



camchat_sockjs(_Conn, init, init) -> io:format("camchat_sockjs(init)~n"), {ok, init};
camchat_sockjs(_Conn, {info, _Info}, State) -> io:format("camchat_sockjs(info)~n"), {ok, State};
camchat_sockjs(Conn, closed, _State) -> 
    rooms:disconnect(Conn, []),
    {ok, killed};

camchat_sockjs(Conn, {recv, Data}, init) ->
    <<"connect:", Room/bitstring>> = Data,
    {ok, Type} = rooms:connect(Room, Conn, []),
    Conn:send(lists:append("connected:", atom_to_list(Type))),
    {ok, connected};
camchat_sockjs(_Conn, {recv, Data}, connected) ->
    <<"streaming:", _Type/bitstring>> = Data,
    {ok, streaming}.
