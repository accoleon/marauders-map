-module(capturenode).
-author("Kevin Xu").
-export([start/0, stop/0, init/2]).
% ExpPrg holds the args for calling tshark - capture filter is excluding beacons, display filter is excluding no mac addresses.
% wlan[0] != 0x80 is to filter out beacon (Access Point) frames, -Y display filter is to only return rows with all 3 fields
% wlan.sa: Source Mac Address wlan.seq: Sequence Number radiotap.dbm_antsignal: signal strength

% Reads settings from capturenode.settings in erlang terms
start() ->
	{ok, Terms} = file:consult("capturenode.settings"),
	[{tshark, ExtPrg}, {servernode, ServerNode}] = Terms,
	spawn(?MODULE, init, [ExtPrg, ServerNode]).

stop() ->
	capturenode ! stop,
	unregister(capturenode).
	
init(ExtPrg, ServerNode) ->
	register(capturenode, self()),
	process_flag(trap_exit, true),
	Port = open_port({spawn, ExtPrg}, [in, exit_status, stream, {line, 255}]),
	loop(Port, ServerNode).
	
loop(Port, ServerNode) ->
	receive
		{Port, Data} ->
			{_,{_,Chunk}} = Data, % strip out unnecessary data
			Tokens = string:tokens(Chunk, " "),
			[MAC, SeqNo, SignalStrength] = Tokens,
			% Sends a tuple to ServerNode with capturenode, MAC address, SS and SeqNo 
			rpc:call(ServerNode, servernode, store, [{node(), MAC, list_to_integer(SignalStrength), list_to_integer(SeqNo)}]),
			loop(Port, ServerNode);
		stop ->
			%Port ! {self(), close},
			erlang:port_close(Port),
			receive
				{Port, closed} ->
					exit(normal)
				end;
			{'EXIT', Port, Reason} ->
				io:format("~p terminated due to ~p", [Port, Reason]),
				exit(port_terminated)
	end.