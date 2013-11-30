%% @author Kevin Xu <jxu@uoregon.edu>
%% @copyright 2013 Team Easy
%% @doc Receiver endpoint for capture nodes
%%
%% Defines a receiver that listens for messages from capturenodes and 
%% prepares them for analysis
%% @end
-module(mm_receiver).
-behaviour (gen_server).
-include_lib("stdlib/include/ms_transform.hrl").
-include("mm_records.hrl").

%% API
-export([
	start_link/0,
	stop/0,
	store/1,
	dump/0
]).

%% gen_server callbacks
-export([
	init/1,
	code_change/3,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2
]).

%% Interval between culling data in milliseconds
-define(AGE_INTERVAL, 10000).

%% @doc Starts the receiver
%% @spec start_link() -> {ok, Pid}
%% where
%%	Pid = pid()
%% @end
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
	
%% @doc Stops the receiver
%% @spec stop() -> ok
%% @end
stop() ->
	gen_server:cast(?MODULE, stop).
	
%% @doc Send data to the receiver
store(Data) ->
	gen_server:cast(?MODULE, {store, Data}).
		
%% Initializes the server	
init([]) ->
	process_flag(trap_exit, true),
	erlang:send_after(?AGE_INTERVAL, self(), age_data),
	{ok, ets:new(?MODULE, [set, named_table, {keypos, #row.hash}]), 0}.

%% @doc gen_server callback for termination - do cleanup here
terminate(Reason, _State) ->
	io:format("mm_receiver terminating: ~p~n", [Reason]),
	ok.
	
%% Default handle_call
handle_call(_Req, _From, State) ->
	{noreply, ok, State}.
	
%% Dump handle
handle_cast(dump, State) ->
	List = ets:tab2list(?MODULE),
	io:format("~p~n", [List]),
	{noreply, State};
	
%% Stop handle
handle_cast(stop, State) ->
	io:format("mm_receiver stopping~n"),
	{stop, ok, State};
	
%% Store handle
handle_cast({store, {From, {MAC, SS, SeqNo}}}, State) ->
	Hash = get_key(MAC, SeqNo),
	NewRow = setelement(get_SS_field(From), #row{hash=Hash, mac=MAC,
		lastupdated=mm_misc:timestamp(secs)}, SS),
	case ets:insert_new(State, NewRow) of
		true -> do_nothing;
		false ->
			case ets:update_element(State, Hash, [
				{get_SS_field(From), SS},
				{#row.lastupdated,
				mm_misc:timestamp(secs)}]) of
				true ->
					[Row] = ets:lookup(State, Hash),
					if 
						is_integer(Row#row.nodeA) andalso 
						is_integer(Row#row.nodeB) andalso 
						is_integer(Row#row.nodeC) -> % collected 3 signal strengths
							% Send row to be analyzed
							mm_analyzer2:analyze(Row),
							
							% Delete the record
							ets:delete(State, Hash);
						true ->
							do_nothing
					end;
				false -> do_nothing
			end
	end,
	{noreply, State}.

%% @doc Culls data older than 10seconds
handle_info(age_data, State) ->
	CurrentTime = mm_misc:timestamp(secs),
	ets:select_delete(State, ets:fun2ms(fun(#row{lastupdated=T}) when
		(CurrentTime - T) > 10 -> true end)),
	%io:format("~p rows culled due to expiry~n", [NumDeleted]),
	erlang:send_after(?AGE_INTERVAL, self(), age_data),
	{noreply, State};

%% Timeout handle
handle_info(timeout, _State) ->
	{noreply, _State}.

%% @doc Code changing handler, not used.
%% @private
code_change(_OldVsn, _State, _Extra) ->
	{ok, _State}.
	
%% Internal functions
	
%% Get a hashed value from MAC address and Sequence Number	
get_key(MAC, SeqNo) ->
	erlang:phash2({MAC, SeqNo}).
	
%% Get the tuple position of the record based on the Field Name (used to shorten code in the insert/update loop)
%% Returns nodeX field
get_SS_field(Field) ->
	case Field of
		nodeA -> #row.nodeA;
		nodeB -> #row.nodeB;
		nodeC -> #row.nodeC
	end.
	
dump() ->
	gen_server:cast(?MODULE, dump).