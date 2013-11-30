-module(mm_ws_handler).
-behaviour(cowboy_websocket_handler).
-include("mm_records.hrl").

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-define(WS_KEY, {pubsub, ws_broadcast}).

init({tcp, http}, _Req, _Opts) ->
	{upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
	gproc:reg({p, l, ?WS_KEY}),
	{ok, Req, undefined_state}.

websocket_handle({text, Msg}, Req, State) ->
	[{event, Event}, {data, Data}] = jsx:decode(Msg, [{labels, attempt_atom}]),
	case Event of
		<<"get_trainers">> ->
			Bin = enc(<<"trainers">>, mm_analyzer2:get_trainers()),
			{reply, {text, Bin}, Req, State};
		<<"get_trained_coords">> ->
			Bin = enc(<<"trained_coords">>, mm_analyzer2:get_trained_coords()),
			{reply, {text, Bin}, Req, State};
		<<"start_training">> ->
			[Trainer, X, Y] = Data,
			case mm_analyzer2:start_training(Trainer, X, Y) of
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
			case mm_analyzer2:end_training(Trainer, X, Y) of
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

websocket_info({training_received, Trainer}, Req, State) ->
	Bin = enc(<<"training_received">>, Trainer),
	{reply, {text, Bin}, Req, State};
websocket_info({timeout, _Ref, Msg}, Req, State) ->
	{reply, {text, Msg}, Req, State};
websocket_info(_Info, Req, State) ->
	{ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
	gproc:unreg({p, l, ?WS_KEY}),
	ok.
	
%% @doc Internal wrapper function to encode erlang to json in a pseudo-RPC form
-spec enc(Event, Data) -> Output when
	Event :: any(),
	Data :: any(),
	Output :: binary().
enc(Event, Data) ->
	jsx:encode([{event, Event}, {data, Data}]).