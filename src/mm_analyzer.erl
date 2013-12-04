%% @author Kevin Xu <jxu@uoregon.edu>
%% @copyright 2013 Team Easy
%% @doc Signal Strength Analyzer and trainer
%%
%% This module maintains a Python port to analyze incoming signal strengths and 
%% output an (X, Y) location, along with training functionality to collect data 
%% used for calibrating the Python port.
-module (mm_analyzer).
-include ("mm_records.hrl").
-include_lib("stdlib/include/qlc.hrl"). % query list comprehensions
-behaviour (gen_server).

%% gen_server callbacks
-export ([
	code_change/3, %
	handle_cast/2, %
	handle_call/3, %
	handle_info/2, %
	init/1, %
	terminate/2 %
]).

%% API
-export ([
	analyze/1,
	analyze_test/0,
	clear_training_data/0,
	dump/0,
	end_training/3,
	get_trained_coords/0,
	get_trainers/0,
	start_link/0,
	start_training/3,
	test_list/0
]).

%% Convenience macro to use as a key for gproc websocket broadcasts
-define (WS_KEY, {pubsub, ws_broadcast}).

%% Timeout interval for training
-define (TIMEOUT_INTERVAL, {10000}).

%% State of the gen_server
-record (state, {python}).

%% Training at x, y by trainer
-record (trainer, {mac, name, x, y}).

%% @doc Starts the analyzer.
%%
%% This function is intended to be called from mm_sup, as part of starting the 
%% mm application.
%% @private
%% @end
-spec start_link() -> Result when
	Result :: {ok, Pid} | ignore | {error, Error},
	Pid :: pid(),
	Error :: {already_started, Pid} | term().
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
	
%% @doc Gets a list of trainers
%%
%% This function returns a hardcoded list of trainers, with their MAC
%% addresses
%% @end
-spec get_trainers() -> list().
get_trainers() ->
	[
		{<<"b8:e8:56:b6:dd:22">>, <<"Kevin's iPhone 5s">>},
		{<<"bc:f5:ac:f6:93:37">>, <<"Juston's Overpowered Nexus 5">>},
		{<<"34:23:ba:45:21:8e">>, <<"Thuc's Phone">>}
	].
	
%% @doc Initializes the analyzer
%%
%% Not part of the public interface
%% @private
%% @end
-spec init([]) -> {ok, State} when
	State :: #state{}.
init([]) ->
	process_flag(trap_exit, true),
	% table of currently training trainers
	trainers = ets:new(trainers, [set, named_table, {keypos, #trainer.mac}]),
	% Open port to python
	PyString = "python -u ../algo/incant_knn.py ../trainingdata.txt",
	PythonPort = open_port({spawn, PyString}, [exit_status, stream, {line, 255}]),
	{ok, #state{python=PythonPort}}.

%% @doc gen_server callback for hot code upgrading - unused
%% @private
code_change(_OldVsn, _State, _Extra) ->
	{ok, _State}.
	
%% @doc gen_server callback for termination - do cleanup here
%% @private
terminate(Reason, #state{python=PythonPort}=_State) ->
	io:format("mm_analyzer terminating: ~p~n", [Reason]),
	port_close(PythonPort),
	ok.
	
%% @doc handle python port exiting
%% @private
handle_info({'EXIT', _From, _Reason}, _State) ->
	{noreply, #state{}};
%% @doc handle python port messages
handle_info({_Port, {data, {eol, Msg}}}, _State) ->
	io:format("python returned: ~p~n", [Msg]),
	[MAC, X, Y] = string:tokens(Msg, " "),
	{DeviceType, DeviceName} = case lists:keyfind(list_to_bitstring(MAC), 1, get_trainers()) of
		{_, Trainer} ->
			{<<"trainer">>, Trainer};
		false ->
			{<<"general">>, erlang:phash2(MAC)}
	end,
	gproc:send({p, l, ?WS_KEY}, {position, [
		{i, erlang:phash2(MAC)},
		{devicetype, DeviceType},
		{name, DeviceName},
		{x, list_to_float(X)},
		{y, list_to_float(Y)}]}),
	{noreply, _State};
handle_info(_Req, _State) ->
	{noreply, _State}.
	
	

%% @doc handle raw rows for analysis
%% @private
handle_cast({analyze, Row}, #state{python=PythonPort} = State) ->
	case ets:lookup(trainers, Row#row.mac) of
		[] -> % not a trainer or currently not training, data to be analyzed
			port_command(PythonPort, io_lib:format("~s ~w ~w ~w~n", [
				Row#row.mac,
				Row#row.nodeA,
				Row#row.nodeB,
				Row#row.nodeC]), [nosuspend]);
		[Trainer] -> % a trainer packet and is currently training
			train(Row, Trainer), % record the packet in db
			gproc:send({p, l, ?WS_KEY}, {training_received, [
				Row#row.mac,
				Trainer#trainer.x,
				Trainer#trainer.y]}) % broadcast to websocket that training
									% is received.
	end,
	{noreply, State}.
	
%% @doc call handler for start_training
%% @private
handle_call({start_training, Trainer, X, Y}, _From, _State) ->
	TrainerLoc = [Trainer, X, Y],
	case lists:keyfind(Trainer, 1, get_trainers()) of
		{MAC, Name} ->
			NewTrainer = #trainer{mac=MAC, name=Name, x=X, y=Y},
			case ets:insert_new(trainers, NewTrainer) of
				true ->
					{reply, {training_started, TrainerLoc}, _State};
				false ->
					{reply, {already_training, TrainerLoc}, _State}
			end;
		false -> % not a valid trainer, ignore request
			{reply, {not_trainer, TrainerLoc}, _State}
	end;
%% @doc call handler for end_training
%% @private
handle_call({end_training, Trainer, X, Y}, _From, _State) ->
	TrainerLoc = [Trainer, X, Y],
	case lists:keyfind(Trainer, 1, get_trainers()) of
		{MAC, _Name} ->
			ets:delete(trainers, MAC),
			train_complete(X, Y),
			{reply, {training_ended, TrainerLoc}, _State};
		false -> % not a valid trainer, ignore request
			{reply, {not_trainer, TrainerLoc}, _State}
	end;
%% @doc call handler for get_trained_coords
%% @private
handle_call(get_trained_coords, _From, _State) ->
	F = fun() ->
		qlc:eval(qlc:q([
			[X, Y] ||
			#mm_trained_coord{
				coord={X, Y},
				complete=Complete} <- mnesia:table(mm_trained_coord),
			Complete =:= true]))
	end,
	List = mnesia:activity(transaction, F),
	{reply, List, _State};
%% @doc call handler for dump
%% @private
handle_call(dump, _From, _State) ->
	Filename = "../trainingdata.txt",
	io:format("Dumping data from Mnesia to ~p~n", [filename:absname(Filename)]),
	List = ets:tab2list(mm_training), % exports mnesia table to list
	{ok ,File} = file:open(Filename, [write]),
	lists:foreach(fun (T) -> % write every item on list into line of File
		io:fwrite(File, "~w,~w,~w,~w,~w,~w~n", [
			T#mm_training.timestamp,
			T#mm_training.x,
			T#mm_training.y,
			T#mm_training.nodeA,
			T#mm_training.nodeB,
			T#mm_training.nodeC])
	end, List),
	{reply, file:close(File), _State};
%% @doc call handler for clear_training_data.
%% @private
handle_call(clear_training_data, _From, _State) ->
	io:format("Clearing data from Mnesia tables...~n"),
	{atomic, _} = mnesia:clear_table(mm_training),
	{atomic, _} = mnesia:clear_table(mm_trained_coord),
	{reply, ok, _State};
%% @doc Default call handler for gen_server
%% @private
handle_call(_Req, _From, State) ->
	{noreply, ok, State}.
	
%% @doc Analyzes a row of signal strengths
-spec analyze(Row) -> ok when
	Row :: #row{}.
analyze(Row) when is_record(Row, row) ->
	gen_server:cast(?MODULE, {analyze, Row}).
	
%% @doc Tests the Analyze function with a dummy row
-spec analyze_test() -> ok.
analyze_test() ->
	Row = #row{hash= 234234242, mac= <<"12345678">>, nodeA=-56, nodeB=-72, nodeC=-68},
	gen_server:cast(?MODULE, {analyze, Row}).
	
test_list() ->
	[
		#row{hash= 234234242, mac= <<"12345678">>, nodeA=-56, nodeB=-72, nodeC=-68}
	].
	
%% @doc helper function to train a packet to a trainer
-spec train(Row, Trainer) -> ok when
	Row :: #row{},
	Trainer :: #trainer{}.
train(Row, Trainer) ->
	F = fun() ->
		mnesia:write(#mm_training{
			timestamp = mm_misc:timestamp(microsecs),
			mac = Row#row.mac, 
			name = Trainer#trainer.name, 
			x = Trainer#trainer.x,
			y = Trainer#trainer.y,
			nodeA = Row#row.nodeA,
			nodeB = Row#row.nodeB,
			nodeC = Row#row.nodeC})
	end,
	mnesia:activity(transaction, F).
	
%% @doc helper function to mark a certain coordinate as complete
-spec train_complete(X, Y) -> ok when
	X :: integer(),
	Y :: integer().
train_complete(X, Y) ->
	F = fun() ->
		mnesia:write(#mm_trained_coord{
			coord={X, Y},
			complete=true})
	end,
	mnesia:activity(transaction, F).

%% @doc helper function to dump training database into a text file
-spec dump() -> ok | {error, Reason} when
	Reason :: file:posix() | badarg | terminated.
dump() ->
	gen_server:call(?MODULE, dump).
	
%% @doc Starts training on packets from Trainer MAC address and XY coordinates.
-spec start_training(Trainer, X, Y) ->
		{training_started, TrainerLoc} | 
		{already_training, TrainerLoc} |
		{not_trainer, TrainerLoc} when
	Trainer :: bitstring(),
	X :: integer(),
	Y :: integer(),
	TrainerLoc :: list().
start_training(Trainer, X, Y) ->
	gen_server:call(?MODULE, {start_training, Trainer, X, Y}).
	
%% @doc Ends training on packets from Trainer MAC address and XY coordinates.
-spec end_training(Trainer, X, Y) -> 
		{training_ended, TrainerLoc } | {not_trainer, TrainerLoc} when
	Trainer :: bitstring(),
	X :: integer(),
	Y :: integer(),
	TrainerLoc :: list().
end_training(Trainer, X, Y) ->
	gen_server:call(?MODULE, {end_training, Trainer, X, Y}).
	
%% @doc Gets a list of coordinates which are already trained.
-spec get_trained_coords() -> list().
get_trained_coords() ->
	gen_server:call(?MODULE, get_trained_coords).
	
%% @doc Clears all training data from Mnesia tables.
-spec clear_training_data() -> ok.
clear_training_data() ->
	gen_server:call(?MODULE, clear_training_data).