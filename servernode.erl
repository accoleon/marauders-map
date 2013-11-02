-module(servernode).
-author("Kevin Xu").
-include_lib("stdlib/include/ms_transform.hrl").
-include("records.hrl").
-export([start/0, stop/0, store/1, trilaterate/0,age_data/0, list/0, time_stamp/0]).

start() ->
	ets:new(rawdata, [bag, named_table, public]),
	ets:new(captureNodeLocations, [set, named_table, public]),
	ets:new(finaldata, [set, named_table, public]),
	timer:apply_interval(10000, servernode, age_data, []),
	register(servernode, spawn(fun() -> loop() end)).
	
stop() ->
	servernode ! stop,
	unregister(servernode),
	ets:delete(rawdata),
	ets:delete(captureNodeLocations),
	ets:delete(finaldata).

store(Value) ->
	servernode ! {self(), {store, Value}}.

loop() ->
	receive
		{_, {store, {FromNode, Mac, SS, SeqNo}}} ->
			%io:format("From: ~p MAC: ~p SS: ~p Seq: ~p~n", [FromNode, Mac, SS, SeqNo]),
			ets:insert(rawdata, #rawrow{mac=Mac, ss=SS, seqno=SeqNo, fromnode=FromNode, lastupdated=time_stamp()}),
			% From ! {servernode, true},
			loop()
	end.
	
% this eventually should spawn a new process to do trilateration for efficiency
trilaterate() ->
	Key = ets:first(rawdata),
	List = ets:lookup(rawdata, Key),
	io:format("key: ~p length of matches: ~p~n", [Key, length(List)]).
	
list() ->
	List = ets:tab2list(rawdata),
	io:format("number of rows in rawdata: ~p~n", [length(List)]).
	
age_data() ->
	% culls rows older than 10s
	CurrentTime = time_stamp(),
	NumDeleted = ets:select_delete(rawdata, ets:fun2ms(fun(#rawrow{lastupdated=T}) when (CurrentTime - T) > 10 -> true end)),
	io:format("~p rawrows culled due to expiry~n", [NumDeleted]),
	list().
	
time_stamp() ->
	{MegaSecs, Secs, _} = os:timestamp(),
	MegaSecs * 1000000 + Secs.