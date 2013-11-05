-module(mmserver_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.
start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/", cowboy_static, {priv_file, mmserver, "index.html"}},
			{"/websocket", mmserver_ws_handler, []},
			{"/static/[...]", cowboy_static, {priv_dir, mmserver, "static"}}
		]}
	]),
	{ok, _} = cowboy:start_http(http, 100, [{port, 8080}],
		[{env, [{dispatch, Dispatch}]}]),
	mmserver_sup:start_link().

stop(_State) ->
	ok.
