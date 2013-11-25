-module(mm_capture).
-author("Kevin Xu").
-export([start/0, stop/0, exit/0, init/3]).
% ExpPrg holds the args for calling tshark - capture filter is excluding beacons, display filter is excluding no mac addresses.
% wlan[0] != 0x80 is to filter out beacon (Access Point) frames, -Y display filter is to only return rows with all 3 fields
% wlan.sa: Source Mac Address wlan.seq: Sequence Number radiotap.dbm_antsignal: signal strength

%% Reads settings from ?MODULE.settings in erlang terms
%% Starts a ?MODULE with default settings BUT with custom node location (nodeA, nodeB, nodeC)
start() ->
	{ok, Terms} = file:consult("mm_capture.settings"),
	[{tshark, ExtPrg}, {mm_receiver, Receiver}, {interface, Interface}, {blacklist, BlackList}, {whitelist, WhiteList}, {cap, ThisNode}] = Terms,
	% Sets up interface, blacklist, whitelist, then redirects stderr to null (prevents annoying counter text)
	String = [ExtPrg, <<" -i ">>, Interface, <<" -f \"wlan[0] != 0x80 ">>, create_blacklist_filter(BlackList), create_whitelist_filter(WhiteList), <<"\" 2> /dev/null">> ],
	% String is probably not flattened, but open_port wants a flat string, not iolist
	Flattened = lists:flatten(io_lib:format("~s", [String])),
	io:format("~s~n", [Flattened]),
	spawn(?MODULE, init, [Flattened, Receiver, ThisNode]).

%% Stop the ?MODULE
stop() ->
	?MODULE ! stop.

%% Exit the ?MODULE
exit() ->
	?MODULE ! exit.
	
%% Initialize the ?MODULE
init(ExtPrg, Receiver, ThisNode) ->
	register(?MODULE, self()),
	process_flag(trap_exit, true),
	Port = open_port({spawn, ExtPrg}, [in, exit_status, stream, {line, 255}]),
	%{os_pid, OsPid} = erlang:port_info(Port, os_pid),
	%io:format("~p~n", [OsPid + 1]),
	loop(Port, Receiver, ThisNode).
	
%% Main ?MODULE loop
loop(Port, Receiver, ThisNode) ->
	receive
		{Port, Data} ->
			{_,{_,Chunk}} = Data, % strip out unnecessary data
			[MAC, SeqNo, SignalStrength] = string:tokens(Chunk, " "),
			% Sends a tuple to Receiver with , MAC address (only the first 17 characters, prevent multiple MACs), SS, SeqNo, and microsecond timestamp
			rpc:cast(Receiver, mm_receiver, store, [{ThisNode, {list_to_bitstring(string:left(MAC, 17)), list_to_integer(SignalStrength), list_to_integer(SeqNo), mm_misc:timestamp(microsecs)}}]),
			loop(Port, Receiver, ThisNode);
		stop ->
			{os_pid, OsPid} = erlang:port_info(Port, os_pid),
			%io:format("~p~n", [OsPid + 1]),
			os:cmd(io_lib:format("kill -15 ~p", [OsPid + 1])),
			erlang:port_close(Port),
			unregister(?MODULE);
		exit ->
			?MODULE ! stop,
			init:stop()
	end.

create_blacklist_filter(BlackList) ->
	[[<<"and not wlan src host ">>, N, <<" ">>] || N <- BlackList].
	
create_whitelist_filter(WhiteList) ->
	[[<<"and wlan src host ">>, N, <<" ">>] || N <- WhiteList].