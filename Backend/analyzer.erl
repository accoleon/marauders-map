-module(analyzer).
-author("Kevin Xu").
-export([start/0, stop/0, store/1, list/0]).

start() ->
	ets:new(rawdata, [bag, named_table]),
	register(analyzer, spawn(fun() -> loop() end)).

stop() ->
	analyzer ! stop,
	unregister(analyzer),
	ets:delete(rawdata).
	
store(Value) ->
	analyzer ! {store, Value}.
	
loop() ->
	receive
		{store, Value} ->
			ets:insert(rawdata, Value),
			loop()
	end.
	
list() ->
	io:format("attempting to list:~n").
			