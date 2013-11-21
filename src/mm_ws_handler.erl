-module(mm_ws_handler).
-behaviour(cowboy_websocket_handler).
-include("mm_records.hrl").

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-define(WSKey, {pubsub, ws_broadcast}).

init({tcp, http}, _Req, _Opts) ->
	{upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
	gproc:reg({p, l, ?WSKey}),
	erlang:start_timer(1000, self(), <<"1150, -10, -10, -10">>),
	{ok, Req, undefined_state}.

websocket_handle({text, Msg}, Req, State) ->
	String = binary_to_list(Msg),
	[Cmd, Args] = string:tokens(String, "^"),
	case Cmd of
		"GET_TRAINERS" ->
			List = ets:tab2list(trainers),
			Bin = jiffy:encode({List}),
			{reply, {text, << "TRAINERS^", Bin/bitstring>>}, Req, State};
		"TRAINING_START" ->
			[Trainer, X, Y] = string:tokens(Args, ","),
			mm_analyzer ! {training_start, Trainer, X , Y},
			{reply, {text, << "TRAINING STARTED">>}, Req, State};
		"TRAINING_END" ->
			mm_analyzer ! {training_end},
			{reply, {text, << "TRAINING ENDED">>}, Req, State};
		"RUBBISH" ->
			%io:format("Rubbish received~n"),
			{ok, Req, State};
		true -> % default for non-recognized text
			{reply, {text, << "That's what she said! ", Msg/binary >>}, Req, State}
	end;
websocket_handle(_Data, Req, State) ->
	{ok, Req, State}.

websocket_info({_Pid, ?WSKey, Msg}, Req, State) ->
	{reply, {text, Msg}, Req, State};
websocket_info({timeout, _Ref, Msg}, Req, State) ->
	%erlang:start_timer(3000, self(), <<"1150, -20, -40, -10">>),
	{reply, {text, Msg}, Req, State};
websocket_info(_Info, Req, State) ->
	{ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
	gproc:unreg({p, l, ?WSKey}),
	ok.