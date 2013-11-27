-module(mm_app).
-behaviour(application).
-include ("mm_records.hrl").

%% API.
-export([start/2, stop/1, install/1]).

%% API.
start(normal, []) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/", cowboy_static, {priv_file, mm, "index.html"}},
			{"/trainer", cowboy_static, {priv_file, mm, "trainer.html"}},
			{"/websocket", mm_ws_handler, []},
			{"/static/[...]", cowboy_static, {priv_dir, mm, "static"}}
		]}
	]),
	{ok, _} = cowboy:start_http(http, 100, [{port, 8080}],
		[{env, [{dispatch, Dispatch}]}]),
	%mnesia:wait_for_tables([mm_training], 5000),
	mm_sup:start_link().

stop(_State) ->
	cowboy:stop_listener(http),
	ok.

-spec install(Nodes) -> ok | {error, Reason} when
	Nodes :: 
install(Nodes) ->
	ok = mnesia:create_schema(Nodes),
	rpc:multicall(Nodes, application, start, [mnesia]),
	mnesia:create_table(
		mm_training,
		[
			{attributes, record_info(fields, mm_training)},
			{index, [#mm_training.timestamp]},
			{disc_copies, Nodes}
		]),
	rpc:multicall(Nodes, application, stop, [mnesia]).