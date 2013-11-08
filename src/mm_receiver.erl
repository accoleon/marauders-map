%%% @author Kevin Xu <jxu@uoregon.edu>
%%% @copyright 2013 Team Easy
%%% @doc Defines a receiver that listens for messages from capturenodes and prepares them for analysis
%%% @end
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
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

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
%% @spec store(Data::tuple()) -> ok.
%% @end
store(Data) ->
	gen_server:cast(?MODULE, {store, Data}).
		
%% Initializes the server	
init([]) ->
	process_flag(trap_exit, true),
	erlang:send_after(?AGE_INTERVAL, self(), age_data),
	{ok, ets:new(?MODULE, [set, named_table, {keypos, #row.hash}]), 0}.

terminate(_Reason, _State) ->
	io:format("mm_receiver terminating~n"),
	ok.
	
%% Default handle_call
handle_call(_Req, _From, State) ->
	{reply, ok, State}.
	
%% Dump handle
handle_cast(dump, State) ->
	List = ets:tab2list(?MODULE),
	io:format("~p~n", [List]),
	{mm_ws_handler, node()} ! {self(), {?MODULE, mm_ws_handler}, <<"Receiver called dump">>},
	{noreply, State};
	
%% Stop handle
handle_cast(stop, State) ->
	io:format("mm_receiver stopping~n"),
	{stop, ok, State};
	
%% Store handle
handle_cast({store, {From, {MAC, SS, SeqNo, MicroTime}}}, State) ->
	Hash = get_key(MAC, SeqNo),
	PreRow = setelement(get_SS_field(From), #row{hash=Hash, mac=MAC, lastupdated=mm_misc:timestamp(secs)}, SS),
	NewRow = setelement(get_time_field(From), PreRow, MicroTime),
	case ets:insert_new(State, NewRow) of
		true -> do_nothing;
		false ->
			case ets:update_element(State, Hash, [{get_SS_field(From), SS}, {get_time_field(From), MicroTime}, {#row.lastupdated, mm_misc:timestamp(secs)}]) of
				true ->
					[Row] = ets:lookup(State, Hash),
					if 
						is_integer(Row#row.nodeA) andalso is_integer(Row#row.nodeB) andalso is_integer(Row#row.nodeC) ->
							% Send row to be analyzed
							{mm_analyzer, node()} ! {data, Row},
							
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
	ets:select_delete(State, ets:fun2ms(fun(#row{lastupdated=T}) when (CurrentTime - T) > 10 -> true end)),
	%io:format("~p rows culled due to expiry~n", [NumDeleted]),
	erlang:send_after(?AGE_INTERVAL, self(), age_data),
	{noreply, State};

%% Timeout handle
handle_info(timeout, _State) ->
	{noreply, _State}.

	
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
	
%% Get the tuple position of the record based on Field Name, returns the nodeXTime field
get_time_field(Field) ->
	case Field of
		nodeA -> #row.nodeATime;
		nodeB -> #row.nodeBTime;
		nodeC -> #row.nodeCTime
	end.
	
dump() ->
	gen_server:cast(?MODULE, dump).