-module(mm_sup).
-behaviour(supervisor).

%% API.
-export([start_link/0]).

%% supervisor.
-export([init/1]).

%% API.

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor.

init([]) ->
	{ok, {{one_for_one, 10, 10}, [
		?CHILD(mm_receiver, worker), 
		?CHILD(mm_analyzer, worker),
		?CHILD(mm_broadcaster, worker)
	]}}.
