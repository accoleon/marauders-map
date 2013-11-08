-module(mm_broadcaster).
-behaviour(gen_server).

%% API
-export([start_link/0, register/1, unregister/1]).

%% gen_server callbacks
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2]).

%% @doc Starts the broadcaster
%% @spec start_link() -> {ok, Pid}
%% where
%%	Pid = pid()
%% @end
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
	
init([]) ->
	process_flag(trap_exit, true),
	{ok, [], 0}.

terminate(_Reason, _State) ->
	io:format("mm_broadcaster terminating~n"),
	ok.

register(Pid) ->
	gen_server:cast(?MODULE, {register, Pid}).
	
unregister(Pid) ->
	gen_server:cast(?MODULE, {unregister, Pid}).
	
code_change(_OldVsn, _State, _Extra) ->
	{ok, _State}.
	
%% Default handle_call
handle_call(_Req, _From, State) ->
	{reply, ok, State}.
	
%% Register handle
handle_cast({register, Pid}, State) ->
	{noreply, [Pid | State]};
%% Unregister handle
handle_cast({unregister, Pid}, State) ->
	{noreply, lists:delete(Pid, State)};
%% Stop handle
handle_cast(stop, State) ->
	io:format("mm_broadcaster stopping~n"),
	{stop, ok, State};
%% Cast handle
handle_cast(_Req, State) ->
	io:format("mm_broadcaster stopping~n"),
	{noreply, State}.
	
%% Timeout handle
handle_info(timeout, _State) ->
	{noreply, _State}.