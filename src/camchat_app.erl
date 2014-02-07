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
                {"/:chatroom", cowboy_static, {file, "priv/www/index.html"}},
                {"/webchat/js/[...]", cowboy_static, {dir, "priv/www/webchat/js/"}},
                {"/webchat/css/[...]", cowboy_static, {dir, "priv/www/css/"}}
              ]
        }
    ].

