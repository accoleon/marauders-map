-module(servernode).
-author("Kevin Xu").
-include_lib("stdlib/include/ms_transform.hrl").
-include("records.hrl").
-export([start_link/0, stop/0, trilaterate/0, age_data/0, list/0, time_stamp/0, get_key/2, get_rawdata/0]).

%% Interval between culling data in milliseconds
age_interval() -> 10000.

%% Start the server
start_link() ->
	ets:new(rawdata, [set, named_table, {keypos, #row.hash}]),
	Pid = spawn(fun() -> loop() end),
	ets:give_away(rawdata, Pid, gift),
	erlang:send_after(age_interval(), Pid, agedata),
	register(servernode, Pid),
	{ok, Pid}.
	
%% Stop the server
stop() ->
	servernode ! stop,
	unregister(servernode).

%% Main server loop
loop() ->
	receive
		{From, {Mac, SS, SeqNo}} ->
			Hash = get_key(Mac, SeqNo),
			NewRow = setelement(get_record_position(From), #row{hash=Hash, lastupdated=time_stamp()}, SS),
			case ets:insert_new(rawdata, NewRow) of
				true -> ok;
				false ->
					ets:update_element(rawdata, Hash, [{get_record_position(From), SS}, {#row.lastupdated, time_stamp()}]),
					ok
			end,
			loop();
		agedata ->
			age_data(),
			loop();
		stop ->
			ets:delete(rawdata)
	end.
	
% this eventually should spawn a new process to do trilateration for efficiency
% 
trilaterate() ->
	Key = ets:first(rawdata),
	List = ets:lookup(rawdata, Key),
	io:format("key: ~p length of matches: ~p~n", [Key, length(List)]).

%% Returns the number of items in rawdata table
list() ->
	List = ets:tab2list(rawdata),
	io:format("rawdata: ~p~n", [length(List)]).
	
%% culls rows older than 10s
age_data() ->
	CurrentTime = time_stamp(),
	NumDeleted = ets:select_delete(rawdata, ets:fun2ms(fun(#row{lastupdated=T}) when (CurrentTime - T) > 10 -> true end)),
	io:format("~p rows culled due to expiry~n", [NumDeleted]),
	erlang:send_after(age_interval(), self(), agedata),
	list().
	
%% Calculates a timestamp based on system timestamp
time_stamp() ->
	{MegaSecs, Secs, _} = os:timestamp(),
	MegaSecs * 1000000 + Secs.
	
%% Get a hashed value from MAC address and Sequence Number	
get_key(Mac, SeqNo) ->
	erlang:phash2({Mac, SeqNo}).
	
%% Get the tuple position of the record based on the Field Name (used to shorten code in the insert/update loop)
get_record_position(Field) ->
	case Field of
		nodeA -> #row.nodeA;
		nodeB -> #row.nodeB;
		nodeC -> #row.nodeC
	end.
	
%% Debugging function to see what's in the table
get_rawdata() ->
	io:format("~p~n", [ets:tab2list(rawdata)]).