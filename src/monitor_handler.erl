-module(monitor_handler).

-export([init/3]).
-export([content_types_provided/2]).
-export([send_stats/2]).

init(_Transport, _Req, []) ->
	{upgrade, protocol, cowboy_rest}.

content_types_provided(Req, State) ->
	{[
		{<<"application/json">>, send_stats}
	], Req, State}.

send_stats(Req, State) ->
	{Type, Req1} = cowboy_req:binding(type, Req),
    {Until, Req2} = cowboy_req:binding(until, Req1),
    {Count, Req3} = cowboy_req:binding(count, Req2),
    Online = rooms:monitor_query(Type, Until, binary_to_integer(Count), 1000),
    Resp = lists:zipwith(
        fun(X, Y) -> 
            {integer_to_binary(X), Y}
        end,
        lists:seq(0,binary_to_integer(Count)),
        Online),
    Body = jiffy:encode({Resp}),
    {Body, Req3, State}.
