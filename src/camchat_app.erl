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
    Port         = port(), 
    TransOpts    = [{port, Port}],
    ProtoOpts    = [{env, [{dispatch, Dispatch}]}],
    NumAcceptors = 100,
    start(http, NumAcceptors, TransOpts, ProtoOpts),
    camchat_sup:start_link().

start(http, NumAcceptors, TransOpts, ProtoOpts) ->
    {ok, _}      = cowboy:start_http(http, NumAcceptors, TransOpts, ProtoOpts);

start(https, NumAcceptors, TransOpts, ProtoOpts) ->
    PrivDir      = "priv",
    CA           = {cacertfile, PrivDir ++ "/ssl/cowboy-ca.crt"},
    Cert         = {certfile, PrivDir ++ "/ssl/server.crt"},
    Key          = {keyfile, PrivDir ++ "/ssl/server.key"},
    TransOpts2   = TransOpts ++ [CA, Cert, Key],
    {ok, _}      = cowboy:start_https(http, NumAcceptors, TransOpts2, ProtoOpts).


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
                {"/", cowboy_static, {file, "priv/www/index.html"}},
                {string:concat(SockjsPath, "/[...]"), sockjs_cowboy_handler, SockjsState},
                {"/:chatroom", cowboy_static, {file, "priv/www/room.html"}},
                {"/webchat/js/[...]", cowboy_static, {dir, "priv/www/webchat/js/"}},
                {"/webchat/css/[...]", cowboy_static, {dir, "priv/www/css/"}},
                {"/webchat/images/[...]", cowboy_static, {dir, "priv/www/images/"}},
                {"/settings_widgets/[...]", cowboy_static, {dir, "priv/www/settings_widgets/"}},
                {"/query/suggest_random_room", random_room_handler, []},
                {"/query/suggest_empty_room", empty_room_handler, []},
                {"/[...]", error_pages, []}
            ]
        }
    ].

port() ->
    case os:getenv("PORT") of
        false ->
            {ok, Port} = application:get_env(http_port),
            Port;
        Other ->
            list_to_integer(Other)
    end.
