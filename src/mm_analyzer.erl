-module(mm_analyzer).
-author("Kevin Xu").
-include("mm_records.hrl").

%% OTP callbacks
-export([init/1, system_continue/3, system_terminate/4, system_code_change/4, write_debug/3]).

-export([start_link/0, trilaterate/7, v_sum/2, calculate_distance/1]).

-define(WSKey, {pubsub, ws_broadcast}).

%% Assume nodes A, B, C are on a bottom left origin 100 x 100 grid
%% A - 21, 20
%% B - 10, 80
%% C - 50, 10

p1() -> [0, 0].
p2() -> [100, 0].
p3() -> [0, 100].

start_link() ->
	proc_lib:start_link(?MODULE, init, [self()]).
	
init(Parent) ->
	register(?MODULE, self()),
	process_flag(trap_exit, true),
	Deb = sys:debug_options([]),
	v_dist(p1(), p2()),
	p3(),
	ets:new(trainers, [set, named_table]),
	ets:insert(trainers, [
		{<<"b8:e8:56:b6:dd:22">>, <<"Kevin's iPhone 5s">>},
		{<<"bc:f5:ac:f6:93:37">>, <<"Juston's Overpowered Nexus 5">>},
		{<<"34:23:ba:45:21:8e">>, <<"Thuc's Phone">>}
	]),
	%io:format("~p~n", [filename:absname("../trainingdata.ets")]),
	case ets:file2tab("../trainingdata.ets") of
		{ok, _} -> do_nothing; % table is retrieved from file
		{error, _} -> % cannot retrieve table, create new one instead
			ets:new(trainingdata, [set, named_table, {keypos, #row.hash}]),
			io:format("created new ets table since file does not exist~n")
	end,
	proc_lib:init_ack(Parent, {ok, self()}),
	loop(Parent, Deb).
	
system_continue(Parent, Deb, _State) ->
	loop(Parent, Deb).
	
system_terminate(Reason, _Parent, _Deb, _State) ->
	io:format("mm_analyzer terminating~n"),
	ets:tab2file(trainingdata, "../trainingdata.ets"),
	exit(Reason).
	
system_code_change(_Misc, _Module, _OldVsn, _Extra) ->
	{ok, _Misc}.
	
write_debug(Dev, Event, Name) ->
    io:format(Dev, "~p event = ~p~n", [Name, Event]).
	
loop(Parent, Deb) ->
	receive
		{data, Data} ->
			Row = Data#row{},
			case ets:lookup(trainers, Row#row.mac) of
				[] -> % not a trainer, data to be analyzed
					trilaterate(Row#row.mac, Row#row.nodeA, Row#row.nodeATime, Row#row.nodeB, Row#row.nodeBTime, Row#row.nodeC, Row#row.nodeCTime);
					%io:format("raw: ~p~n", [Row]),
					%gproc:send({p,l,?WSKey}, {self(), ?WSKey, io_lib:format("~p,~p,~p,~p~n", [Row#row.hash, Row#row.nodeA, Row#row.nodeB, Row#row.nodeC])});
					%gproc:send({p, l ,?WSKey}, {self(), ?WSKey, io_lib:format("~s,~s,~s,~s~n", [Row#row.hash, Row#row.nodeA, Row#row.nodeB, Row#row.nodeC])});
				[{_MAC, Name}] -> % a trainer device
					gproc:send({p,l,?WSKey}, {self(), ?WSKey, io_lib:format("~p,~p,~p,~p~n", [Name, Row#row.nodeA, Row#row.nodeB, Row#row.nodeC])}),
					gproc:send({p,l,?WSKey}, {self(), ?WSKey, io_lib:format("~p distances: ~fm ~fm ~fm~n", [Name, calculate_distance(Row#row.nodeA), calculate_distance(Row#row.nodeB), calculate_distance(Row#row.nodeC)])}),
					ets:insert(trainingdata, Row)
			end,			
			%io:format("Received ~p~n", [Row]),
			loop(Parent, Deb);
		{'EXIT', _From, Reason} ->
			system_terminate(Reason, Parent, Deb, undefined);
		{system, From, Request} ->
			sys:handle_system_msg(Request, From, Parent, ?MODULE, Deb, undefined)
	end.
	
trilaterate(MAC, R1, R1Time, R2, R2Time, R3, R3Time) ->
	List = [{nodeA, R1, R1Time}, {nodeB, R2, R2Time}, {nodeC, R3, R3Time}],
	TimeSortedList = lists:keysort(3, List), % sort list by time
	[{_Node, _X, FirstTime} | _Tail ] = TimeSortedList,
	_NormalizedList = lists:map(fun ({Node, X, Y}) -> {Node, X, Y-FirstTime} end, TimeSortedList),
	%io:format("Mac: ~p Data: ~p~n", [MAC, NormalizedList]),
	{MAC, R1, R1Time, R2, R2Time, R3, R3Time}.
	
calculate_distance(InputDb) ->
	% InputFreq = 2412, % Frequency for channel 1 of wifi 2.4ghz spectrum in mhz
	% Exp = (27.55 - (20 * math:log10(InputFreq)) - InputDb) / 20.0,
	% math:pow(10.0, Exp).
	K = -36,
	math:exp((K-InputDb)/20).

v_dist(V1, V2) ->
	V2Negative = lists:map(fun (X) -> -X end, V2),
	V3 = v_sum(V1, V2Negative),
	VSquared = lists:map(fun (X) -> math:pow(X, 2) end, V3),
	N = lists:sum(VSquared),
	math:sqrt(N).

v_sum(L1, L2) -> v_sum(L1, L2, []).
v_sum([], [], Acc) -> Acc;
v_sum([H1 | T1], [H2 | T2], Acc) ->
	v_sum(T1, T2, [H1 + H2 | Acc]).