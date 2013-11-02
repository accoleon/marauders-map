-module(servernode).
-export([start/0, stop/0, store/1]).

start() ->
	register(servernode, spawn(fun() -> loop() end)).
stop() ->
	servernode ! stop.

store(Value) ->
	servernode ! {self(), {store, Value}}.

loop() ->
	receive
		{From, {store, {FromNode, Mac, SS, SeqNo}}} ->
			io:format("From: ~p Mac: ~p SS: ~p Seq: ~p~n", [FromNode, Mac, SS, SeqNo]),
			From ! {servernode, true},
			loop()
	end.