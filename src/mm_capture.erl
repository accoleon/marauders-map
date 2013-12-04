%% @author Kevin Xu <jxu@uoregon.edu>
%% @copyright 2013 Team Easy
%% @doc Capture Node
%%
%% This module maintains a tshark port to capture wireless packets in the air 
%% and send them to the Receiver.
%% 
%% It is meant to be used with the build script startnode.sh, and not meant to
%% be called directly by any other module.
-module(mm_capture).
-author("Kevin Xu").
-export([start/0, stop/0, exit/0, init/3]).
% ExpPrg holds the args for calling tshark - capture filter is excluding beacons, display filter is excluding no mac addresses.
% wlan[0] != 0x80 is to filter out beacon (Access Point) frames, -Y display filter is to only return rows with all 3 fields
% wlan.sa: Source Mac Address wlan.seq: Sequence Number radiotap.dbm_antsignal: signal strength

%% @doc Starts the capture node using mm_capture.settings as the configuration
%%	file
-spec start() -> Pid when
	Pid :: pid().
start() ->
	{ok, Terms} = file:consult("mm_capture.settings"), [
		{tshark, ExtPrg},
		{mm_receiver, Receiver},
		{interface, Interface},
		{blacklist, BlackList}, 
		{whitelist_enabled, WhiteListEnabled},
		{whitelist, WhiteList},
		{cap, ThisNode}] = Terms,
	% Sets up interface, blacklist, whitelist, then redirects stderr to null (prevents annoying counter text)
	String = case WhiteListEnabled of
		true ->
			[ExtPrg, <<" -i ">>, Interface, <<" -f \"wlan[0] != 0x80 ">>, create_blacklist_filter(BlackList), create_whitelist_filter(WhiteList), <<"\" 2> /dev/null">> ];	
		false ->
			[ExtPrg, <<" -i ">>, Interface, <<" -f \"wlan[0] != 0x80 ">>, create_blacklist_filter(BlackList), <<"\" 2> /dev/null">> ]
	end,
	% String is probably not flattened, but open_port wants a flat string, not iolist
	Flattened = lists:flatten(io_lib:format("~s", [String])),
	io:format("~s~n", [Flattened]),
	spawn(?MODULE, init, [Flattened, Receiver, ThisNode]).

%% @doc Performs a graceful shutdown of the tshark port
-spec stop() -> ok.
stop() ->
	?MODULE ! stop,
	ok.

%% @doc Exits the Erlang node the capture node is running on
%%
%% Performs stop() first for a graceful shutdown of the tshark port.
-spec exit() -> ok.
exit() ->
	?MODULE ! exit,
	ok.
	
%% @doc Initializes the the capture node
%%
%% Called by start() with ExtPrg as the command string for tshark, Receiver 
%% as the Receiver's node PID, and ThisNode as the current node PID.
-spec init(ExtPrg, Receiver, ThisNode) -> ok | true when
	ExtPrg :: string(),
	Receiver :: atom(),
	ThisNode :: pid().
init(ExtPrg, Receiver, ThisNode) ->
	register(?MODULE, self()),
	process_flag(trap_exit, true),
	Port = open_port({spawn, ExtPrg}, [exit_status, stream, {line, 255}]),
	net_adm:ping(Receiver), % sets up connection to receiver
	loop(Port, Receiver, ThisNode).
	
%% @doc Main loop
%%
%% Receives output from tshark from its stdout, packages it into a tuple
%% {ThisNode, {MAC, SignalStrength, SeqNo}} and sends it to the Receiver.
%% MAC is the MAC address of the wireless packet, SignalStrength is the signal
%% strength returned by the IEEE802.11 AVS Radio Header, and SeqNo is the
%% sequence number of the wireless packet.
%% Also handles stop and exit messages for graceful shutdown.
loop(Port, Receiver, ThisNode) ->
	receive
		{Port, Data} ->
			{_,{_,Chunk}} = Data, % strip out unnecessary data
			[MAC, SeqNo, SignalStrength] = string:tokens(Chunk, " "),
			% Sends a tuple to Receiver with , MAC address (only the first 17 characters, prevent multiple MACs), SS, SeqNo, and microsecond timestamp
			rpc:cast(Receiver, mm_receiver, store, [{ThisNode, {list_to_bitstring(string:left(MAC, 17)), list_to_integer(SignalStrength), list_to_integer(SeqNo)}}]),
			loop(Port, Receiver, ThisNode);
		stop ->
			{os_pid, OsPid} = erlang:port_info(Port, os_pid),
			%io:format("~p~n", [OsPid + 1]),
			"" = os:cmd(io_lib:format("kill -15 ~p", [OsPid + 1])),
			erlang:port_close(Port),
			unregister(?MODULE);
		exit ->
			?MODULE ! stop,
			init:stop()
	end.

%% @doc Creates a blacklist filter using a list comprehension on the terms read
%% in from the configuration file mm_capture.settings
create_blacklist_filter(BlackList) ->
	[[<<"and not wlan src host ">>, N, <<" ">>] || N <- BlackList].
	
%% @doc Creates a whitelist filter using a list comprehension on the terms read
%% in from the configuration file mm_capture.settings
create_whitelist_filter(WhiteList) ->
	[[<<"and wlan src host ">>, N, <<" ">>] || N <- WhiteList].