-module(camchat).
-export([start/0]).

start() ->
    ok = application:start(crypto),
    ok = application:start(ranch),
    ok = application:start(cowlib),
    ok = application:start(cowboy),
    ok = application:start(xmerl),
    ok = application:start(sockjs),
    ok = application:start(camchat).
