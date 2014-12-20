-module(rps_app).

-include("rps_ranch.hrl").

-behaviour(application).

-export([start/2, stop/1]).



start(_StartType, _StartArgs) ->
	Res = rps_sup:start_link(),
	{ok, ListenerPid} = ranch:start_listener(test, ?ACCEPTORS, ranch_tcp, [{port, ?PORT}], rps_connect, []),
	io:format("(~p): ~p ranch listeners started at port ~p~n", [ListenerPid, ?ACCEPTORS, ?PORT]),
	Res.

stop(_State) -> ok.
