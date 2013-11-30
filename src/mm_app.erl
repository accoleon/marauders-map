%% @author Kevin Xu <jxu@uoregon.edu>
%% @copyright 2013 Team Easy
%% @doc Application and basic Cowboy HTTP server
%%
%% This module provides a basic Cowboy HTTP and WebSocket server, also acting
%% as the entry point to the entire MM application.
-module(mm_app).
-behaviour(application).
-include ("mm_records.hrl").

%% Application callbacks
-export ([start/2, stop/1]).

%% API.
-export([install/1]).

%% @doc Starts MM server.
%%
%% This function starts the MM application and also starts the top level
%% supervisor.
-spec start(StartType, StartArgs) -> {ok, Pid} | {ok, Pid, State} when
	StartType :: normal | atom(),
	StartArgs :: [any()],
	Pid :: pid(),
	State :: any().
start(normal, []) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/", cowboy_static, {priv_file, mm, "map.html"}},
			{"/trainer", cowboy_static, {priv_file, mm, "trainer.html"}},
			{"/websocket", mm_ws_handler, []},
			{"/static/[...]", cowboy_static, {priv_dir, mm, "static"}}
		]}
	]),
	{ok, _} = cowboy:start_http(http, 100, [{port, 8080}],
		[{env, [{dispatch, Dispatch}]}]),
	mnesia:wait_for_tables([mm_training, mm_trained_coord], 5000),
	mm_sup:start_link().

%% @doc Called after MM server stops.
%%
%% This function is called AFTER the application is stopped by the supervisor.
%% Cleanup can be done here.
-spec stop(State) -> ok when
	State :: any().
stop(_State) ->
	cowboy:stop_listener(http),
	ok.

%% @doc Installs Mnesia tables required for training functionality.
%%
%% Can be ran on either a single node or a cluster of nodes.
-spec install(Nodes) -> ok when
	Nodes :: [node()].
install(Nodes) when is_list(Nodes) ->
	rpc:multicall(Nodes, application, stop, [mnesia]),
	ok = mnesia:create_schema(Nodes),
	rpc:multicall(Nodes, application, start, [mnesia]),
	mnesia:create_table(
		mm_training,
		[
			{attributes, record_info(fields, mm_training)},
			{disc_copies, Nodes},
			{type, ordered_set}
		]),
	mnesia:create_table(
		mm_trained_coord,
		[
			{attributes, record_info(fields, mm_trained_coord)},
			{disc_copies, Nodes},
			{type, set}
		]),
	rpc:multicall(Nodes, application, stop, [mnesia]),
	ok.