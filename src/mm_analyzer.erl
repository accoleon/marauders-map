-module(mm_analyzer).
-author("Kevin Xu").
-include("mm_records.hrl").

%% OTP callbacks
-export([init/1, system_continue/3, system_terminate/4, system_code_change/4, write_debug/3]).

-export([start_link/0, dump/0, test/0]).

-define(WSKey, {pubsub, ws_broadcast}).
-define(TIMEOUT_INTERVAL, 10000). % training timeout in milliseconds

-record(state, {python, is_training, trainer, x, y}).
-record(training, {timestamp, mac, name, x, y, nodeA, nodeB, nodeC}).

start_link() ->
	proc_lib:start_link(?MODULE, init, [self()]).
	
init(Parent) ->
	register(?MODULE, self()),
	process_flag(trap_exit, true),
	Deb = sys:debug_options([]),
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
			ets:new(trainingdata, [ordered_set, named_table, {keypos, #training.timestamp}]),
			io:format("created new ets table since file does not exist~n")
	end,
	% open port to python analyzer
	proc_lib:init_ack(Parent, {ok, self()}),
	State = #state{is_training=false},
	loop(Parent, Deb, State).
	
system_continue(Parent, Deb, State) ->
	loop(Parent, Deb, State).
	
system_terminate(Reason, _Parent, _Deb, _State) ->
	io:format("mm_analyzer terminating: ~p~n", [Reason]),
	%python:stop(State#state.python),
	ets:tab2file(trainingdata, "../trainingdata.ets"),
	?MODULE:dump(),
	exit(Reason).
	
system_code_change(_Misc, _Module, _OldVsn, _Extra) ->
	{ok, _Misc}.
	
write_debug(Dev, Event, Name) ->
    io:format(Dev, "~p event = ~p~n", [Name, Event]).
	
dump() ->
	List = ets:tab2list(trainingdata),
	{ok, File} = file:open("../trainingdata.txt", [write]),
	lists:foreach(fun (T) -> io:fwrite(File, "~w,~w,~w,~w,~w,~w~n", [T#training.timestamp, T#training.x, T#training.y, T#training.nodeA, T#training.nodeB, T#training.nodeC]) end, List),
	file:close(File).
	
test() ->
	{ok, P} = python:start([{python_path, "../algo"}]),
	io:format("~p~n", [python:call(P, operator, add, [2, 2])]).
	
loop(Parent, Deb, State) ->
	receive
		{data, Data} ->
			Row = Data#row{},
			case ets:lookup(trainers, Row#row.mac) of
				[] -> % not a trainer, data to be analyzed
					io:format("~p~n", [python:call(py, operator, add, [2, 2])]);
					%erlang:port_command(State#state.port, io_lib:format("~s ~s ~s ~s ~s~n", [Row#row.mac, Row#row.nodeA, Row#row.nodeB, Row#row.nodeC])),
					%io:format("raw: ~p~n", [Row]),
					%gproc:send({p,l,?WSKey}, {self(), ?WSKey, io_lib:format("~p,~p,~p,~p~n", [Row#row.hash, Row#row.nodeA, Row#row.nodeB, Row#row.nodeC])});
				[{_MAC, Name}] -> % a trainer device
					%io:format("~p ~p ~p~n", [State, Row, Name]),
					train(State, Row, Name)
			end,			
			%io:format("Received ~p~n", [Row]),
			loop(Parent, Deb, State);
		{_Port, _Data} -> % receives data from the port
			%{_,{_,Chunk}} = Data, % strip out unnecessary data
			% assume data comes back line-by-line in the format: MAC X Y
			%[MAC, X, Y] = string:tokens(Chunk, " "), 
			ok;
		{training_start, Trainer, X, Y} ->
			NewState = #state{is_training=true, trainer = list_to_bitstring(Trainer), x=list_to_integer(X), y=list_to_integer(Y)},
			io:format("Training started with ~p~n", [NewState]),
			%erlang:send_after(?TIMEOUT_INTERVAL, ?MODULE, {training_end}),
			loop(Parent, Deb, NewState);
		{training_end} ->
			NewState = #state{is_training=false},
			io:format("Training ended with ~p~n", [State]),
			loop(Parent, Deb, NewState);
		{'EXIT', From, Reason} ->
			io:format("From: ~p Reason: ~p~n", [From, Reason]),
			system_terminate(Reason, Parent, Deb, undefined);
		{system, From, Request} ->
			sys:handle_system_msg(Request, From, Parent, ?MODULE, Deb, undefined)
	end.
	
train(State=#state{is_training=true}, Row, Name) when State#state.trainer == Row#row.mac ->
	#state{trainer=Trainer, x=X, y=Y} = State,
	%io:format("is training; ~p~n", [State]),
	NewTraining = #training{timestamp=mm_misc:timestamp(microsecs), mac=Trainer, name=Name, x=X, y=Y, nodeA=Row#row.nodeA, nodeB=Row#row.nodeB, nodeC=Row#row.nodeC},
	ets:insert(trainingdata, NewTraining),
	gproc:send({p,l,?WSKey}, {self(), ?WSKey, io_lib:format("TRAINING_RECEIVED^~p,~p,~p,~p,~p,~p~n", [Name, X, Y, Row#row.nodeA, Row#row.nodeB, Row#row.nodeC])}),
	ok;
train(#state{is_training=false}, _Row, _Name) ->
	ok.
	%gproc:send({p,l,?WSKey}, {self(), ?WSKey, io_lib:format("~p,~p,~p,~p~n", [Name, Row#row.nodeA, Row#row.nodeB, Row#row.nodeC])}).
	%gproc:send({p,l,?WSKey}, {self(), ?WSKey, io_lib:format("~p distances: ~fm ~fm ~fm~n", [Name, calculate_distance(Row#row.nodeA), calculate_distance(Row#row.nodeB), calculate_distance(Row#row.nodeC)])}).