%% @author Kevin Xu <jxu@uoregon.edu>
%% @copyright 2013 Team Easy
%% @doc OTP Supervisor
%%
%% This module maintains an OTP supervisor that monitors the Receiver and
%% Analyzer components of the Marauder's App application. It conforms to 
%% Erlang's OTP behaviors to ensure error logging, remote monitoring, and 
%% automatic restarts of failed nodes.

-module(mm_sup).
-behaviour(supervisor).

%% API.
-export([start_link/0]).

%% supervisor.
-export([init/1]).

%% API.

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% @doc Starts the supervisor
%%
%% Called by mm_app, which is the first entry point of the Marauder's Map application.
-spec start_link() -> ignore | {error, Reason} | {ok, pid()} when
	Reason :: string().
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Initializes Receiver and Analzyer components as child processes
init([]) ->
	{ok, {{one_for_one, 10, 10}, [
		?CHILD(mm_receiver, worker),
		?CHILD(mm_analyzer, worker)
	]}}.