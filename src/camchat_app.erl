-module(camchat_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    rooms:start(),
    Routes       = routes(),
    Dispatch     = cowboy_router:compile(Routes),
    Port         = 8080, 
    TransOpts    = [{port, Port}],
    ProtoOpts    = [{env, [{dispatch, Dispatch}]}],
    NumAcceptors = 100,
    {ok, _}      = cowboy:start_http(http, NumAcceptors, TransOpts, ProtoOpts),
    io:format("camchat_app:start() Routes:    ~p~n", [Routes]),
    io:format("camchat_app:start() Dispatch:  ~p~n", [Dispatch]),
    io:format("camchat_app:start() Port:      ~p~n", [Port]),
    io:format("camchat_app:start() TransOpts: ~p~n", [TransOpts]),
    io:format("camchat_app:start() ProtoOpts: ~p~n", [ProtoOpts]),
    camchat_sup:start_link().

stop(_State) ->
    ok.

%% ===================================================================
%% Internal functions
%% ===================================================================

routes() ->
    SockjsPath = "/sockjs/camchat",
    SockjsState = sockjs_handler:init_state(list_to_binary(SockjsPath), 
                       fun camchat_sockjs:camchat_sockjs/3, init, []),
    [
        {'_', [
                {"/", camchat_handler, []},
                {string:concat(SockjsPath, "/[...]"), sockjs_cowboy_handler, SockjsState},
                {"/:chatroom", cowboy_static, {priv_file, camchat, "www/index.html"}},
                {"/webchat/js/[...]", cowboy_static, {priv_dir, camchat, "www/webchat/js/"}}
              ]
        }
    ].

