-module(mm_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.
start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/", cowboy_static, {priv_file, mm, "index.html"}},
			{"/websocket", mm_ws_handler, []},
			{"/static/[...]", cowboy_static, {priv_dir, mm, "static"}}
		]}
	]),
	{ok, _} = cowboy:start_http(http, 100, [{port, 8080}],
		[{env, [{dispatch, Dispatch}]}]),
	mm_sup:start_link().

stop(_State) ->
	cowboy:stop_listener(http),
	ok.
