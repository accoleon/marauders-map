%% @author Kevin Xu <jxu@uoregon.edu>
%% @copyright 2013 Team Easy
%% @doc WebSocket Server
%%
%% This module maintains a WebSocket server which provides the backend services
%% for Map display and Trainer tool.
-module(mm_ws_handler).
-behaviour(cowboy_websocket_handler).
-include("mm_records.hrl").

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-define(WS_KEY, {pubsub, ws_broadcast}).

%% @doc Sets up initial parameters for a WebSocket process

%% Called by the cowboy router to handle WebSocket requests and upgrades the
%% TCP connection to a WebSocket connection.
init({tcp, http}, _Req, _Opts) ->
	{upgrade, protocol, cowboy_websocket}.

%% @doc Initializes a WebSocket process
%%
%% Initializes a WebSocket process and registers itself as a subscriber to
%% gproc's pubsub service, to receive broadcasts of data from the Analyzer.
websocket_init(_TransportName, Req, _Opts) ->
	gproc:reg({p, l, ?WS_KEY}),
	%erlang:send_after(1000, self(), {iterate, 1}),
	{ok, Req, undefined_state}.

%% @doc Main server handler
%%
%% Handles events from the frontent clients: Map and Trainer tool. Events and
%% data are packed into JSON by the jsx library.
websocket_handle({text, Msg}, Req, State) ->
	[{event, Event}, {data, Data}] = jsx:decode(Msg, [{labels, attempt_atom}]),
	case Event of
		<<"get_trainers">> ->
			Bin = enc(<<"trainers">>, mm_analyzer:get_trainers()),
			{reply, {text, Bin}, Req, State};
		<<"get_trained_coords">> ->
			Bin = enc(<<"trained_coords">>, mm_analyzer:get_trained_coords()),
			{reply, {text, Bin}, Req, State};
		<<"start_training">> ->
			[Trainer, X, Y] = Data,
			case mm_analyzer:start_training(Trainer, X, Y) of
				{training_started, TrainerLoc} ->
					Bin = enc(<<"training_started">>, TrainerLoc),
					{reply, {text, Bin}, Req, State};
				{already_training, TrainerLoc} ->
					Bin = enc(<<"already_training">>, TrainerLoc),
					{reply, {text, Bin}, Req, State};
				{not_trainer, TrainerLoc} ->
					Bin = enc(<<"not_trainer">>, TrainerLoc),
					{reply, {text, Bin}, Req, State}
			end;
		<<"end_training">> ->
			[Trainer, X, Y] = Data,
			case mm_analyzer:end_training(Trainer, X, Y) of
				{training_ended, TrainerLoc} ->
					Bin = enc(<<"training_ended">>, TrainerLoc),
					{reply, {text, Bin}, Req, State};
				{not_trainer, TrainerLoc} ->
					Bin = enc(<<"not_trainer">>, TrainerLoc),
					{reply, {text, Bin}, Req, State}
			end;
		_ -> % default for non-recognized text
			{ok, Req, State}
	end;
websocket_handle(_Data, Req, State) ->
	{ok, Req, State}.

%% @doc Handles position data from the Analyzer
%%
%% Called when the Analyzer sends location data on a device. The positional data
%% is packed into JSON and sent to the Map
websocket_info({position, Position}, Req, State) ->
	Bin = enc(<<"position">>, [Position]),
	{reply, {text, Bin}, Req, State};
%% @doc Handles training data from the Analyzer
%%
%% Called when the Analyzer sends an acknowledgement that it has successfully
%% received a packet of data from the Trainer tool. Acknowledgement of the 
%% location of the packet is packed into JSON and sent to the Trainer tool.
websocket_info({training_received, Trainer}, Req, State) ->
	Bin = enc(<<"training_received">>, Trainer),
	{reply, {text, Bin}, Req, State};
%% @doc Sends static test data to the client, 2 rows at a time
websocket_info({iterate, Position}, Req, State) when Position =< 66 ->
	erlang:send_after(1000, self(), {iterate, Position + 2}),
	Bin = enc(<<"position">>, [lists:nth(Position, get_test()), lists:nth(Position + 1, get_test())]),
	{reply, {text, Bin}, Req, State};
websocket_info({iterate, _Position}, Req, State) ->
	erlang:send(self(), {iterate, 1}),
	{ok, Req, State};
%% @doc Default info handler - does nothing
websocket_info(_Info, Req, State) ->
	{ok, Req, State}.

%% @doc Called when the WebSocket connection is ended
%%
%% Unsubscribes itself from the publish-subscribe service provided by gproc
%% when the WebSocket session ends.
websocket_terminate(_Reason, _Req, _State) ->
	gproc:unreg({p, l, ?WS_KEY}),
	ok.

%% @doc Contains static test data
%%
%% For internal use only, provides a well-defined set of coordinates and locations to the Map tool for debugging purposes.
get_test() ->
	[
		[{i, 1}, {name, <<"Tester">>}, {x, 0}, {y, 0}],
		[{i, 2}, {name, <<"Kevin">>}, {x, 20}, {y, 30}],
		[{i, 1}, {name, <<"Tester">>}, {x, 1}, {y, 0}],
		[{i, 2}, {name, <<"Kevin">>}, {x, 19}, {y, 29}],
		[{i, 1}, {name, <<"Tester">>}, {x, 2}, {y, 0}],
		[{i, 2}, {name, <<"Kevin">>}, {x, 18}, {y, 28}],
		[{i, 1}, {name, <<"Tester">>}, {x, 3}, {y, 0}],
		[{i, 2}, {name, <<"Kevin">>}, {x, 17}, {y, 27}],
		[{i, 1}, {name, <<"Tester">>}, {x, 4}, {y, 0}],
		[{i, 2}, {name, <<"Kevin">>}, {x, 16}, {y, 26}],
		[{i, 1}, {name, <<"Tester">>}, {x, 5}, {y, 0}],
		[{i, 2}, {name, <<"Kevin">>}, {x, 15}, {y, 25}],
		[{i, 1}, {name, <<"Tester">>}, {x, 6}, {y, 0}],
		[{i, 2}, {name, <<"Kevin">>}, {x, 14}, {y, 24}],
		[{i, 1}, {name, <<"Tester">>}, {x, 7}, {y, 0}],
		[{i, 2}, {name, <<"Kevin">>}, {x, 13}, {y, 23}],
		[{i, 1}, {name, <<"Tester">>}, {x, 8}, {y, 0}],
		[{i, 2}, {name, <<"Kevin">>}, {x, 12}, {y, 22}],
		[{i, 1}, {name, <<"Tester">>}, {x, 9}, {y, 0}],
		[{i, 2}, {name, <<"Kevin">>}, {x, 11}, {y, 21}],
		[{i, 1}, {name, <<"Tester">>}, {x, 10}, {y, 0}],
		[{i, 2}, {name, <<"Kevin">>}, {x, 10}, {y, 20}],
		[{i, 1}, {name, <<"Tester">>}, {x, 11}, {y, 0}],
		[{i, 2}, {name, <<"Kevin">>}, {x, 9}, {y, 19}],
		[{i, 1}, {name, <<"Tester">>}, {x, 12}, {y, 0}],
		[{i, 2}, {name, <<"Kevin">>}, {x, 8}, {y, 18}],
		[{i, 1}, {name, <<"Tester">>}, {x, 13}, {y, 0}],
		[{i, 2}, {name, <<"Kevin">>}, {x, 7}, {y, 17}],
		[{i, 1}, {name, <<"Tester">>}, {x, 14}, {y, 0}],
		[{i, 2}, {name, <<"Kevin">>}, {x, 6}, {y, 16}],
		[{i, 1}, {name, <<"Tester">>}, {x, 15}, {y, 0}],
		[{i, 2}, {name, <<"Kevin">>}, {x, 5}, {y, 15}],
		[{i, 1}, {name, <<"Tester">>}, {x, 16}, {y, 0}],
		[{i, 2}, {name, <<"Kevin">>}, {x, 4}, {y, 14}],
		[{i, 1}, {name, <<"Tester">>}, {x, 17}, {y, 0}],
		[{i, 2}, {name, <<"Kevin">>}, {x, 3}, {y, 13}],
		[{i, 1}, {name, <<"Tester">>}, {x, 18}, {y, 0}],
		[{i, 2}, {name, <<"Kevin">>}, {x, 2}, {y, 12}],
		[{i, 1}, {name, <<"Tester">>}, {x, 19}, {y, 0}],
		[{i, 2}, {name, <<"Kevin">>}, {x, 1}, {y, 11}],
		[{i, 1}, {name, <<"Tester">>}, {x, 20}, {y, 0}],
		[{i, 2}, {name, <<"Kevin">>}, {x, 0}, {y, 10}],
		[{i, 1}, {name, <<"Tester">>}, {x, 19}, {y, 1}],
		[{i, 2}, {name, <<"Kevin">>}, {x, 1}, {y, 9}],
		[{i, 1}, {name, <<"Tester">>}, {x, 18}, {y, 2}],
		[{i, 2}, {name, <<"Kevin">>}, {x, 3}, {y, 8}],
		[{i, 1}, {name, <<"Tester">>}, {x, 17}, {y, 3}],
		[{i, 2}, {name, <<"Kevin">>}, {x, 5}, {y, 7}],
		[{i, 1}, {name, <<"Tester">>}, {x, 16}, {y, 4}],
		[{i, 2}, {name, <<"Kevin">>}, {x, 7}, {y, 6}],
		[{i, 1}, {name, <<"Tester">>}, {x, 15}, {y, 5}],
		[{i, 2}, {name, <<"Kevin">>}, {x, 8}, {y, 5}],
		[{i, 1}, {name, <<"Tester">>}, {x, 14}, {y, 6}],
		[{i, 2}, {name, <<"Kevin">>}, {x, 10}, {y, 4}],
		[{i, 1}, {name, <<"Tester">>}, {x, 13}, {y, 7}],
		[{i, 2}, {name, <<"Kevin">>}, {x, 12}, {y, 3}],
		[{i, 1}, {name, <<"Tester">>}, {x, 12}, {y, 8}],
		[{i, 2}, {name, <<"Kevin">>}, {x, 14}, {y, 2}],
		[{i, 1}, {name, <<"Tester">>}, {x, 11}, {y, 9}],
		[{i, 2}, {name, <<"Kevin">>}, {x, 10}, {y, 1}],
		[{i, 1}, {name, <<"Tester">>}, {x, 10}, {y, 10}],
		[{i, 2}, {name, <<"Kevin">>}, {x, 8}, {y, 0}],
		[{i, 1}, {name, <<"Tester">>}, {x, 9}, {y, 11}],
		[{i, 2}, {name, <<"Kevin">>}, {x, 14}, {y, 0}],
		[{i, 1}, {name, <<"Tester">>}, {x, 8}, {y, 12}],
		[{i, 2}, {name, <<"Kevin">>}, {x, 15}, {y, 1}]
	].
	
%% @doc Internal wrapper function to encode erlang to json in a pseudo-RPC form
-spec enc(Event, Data) -> Output when
	Event :: any(),
	Data :: any(),
	Output :: binary().
enc(Event, Data) ->
	jsx:encode([{event, Event}, {data, Data}]).