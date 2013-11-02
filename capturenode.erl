-module(capturenode).
-author("Kevin Xu").
-export([start/0, stop/0, init/1]).
% ExpPrg holds the args for calling tshark - capture filter is excluding beacons, display filter is excluding no mac addresses.
% wlan[0] != 0x80 is to filter out beacon (Access Point) frames, -Y display filter is to only return rows with all 3 fields
% wlan.sa: Source Mac Address wlan.seq: Sequence Number radiotap.dbm_antsignal: signal strength
start() ->
	ExpPrg = "sudo tshark -f \"wlan[0] != 0x80\" -Y \"wlan.sa && wlan.seq && radiotap.dbm_antsignal\" -I -N m -Tfields -E separator=/s -e wlan.sa -e wlan.seq -e radiotap.dbm_antsignal -l",
	spawn(?MODULE, init, [ExpPrg]).

stop() ->
	capturenode ! stop.
	
init(ExtPrg) ->
	register(capturenode, self()),
	process_flag(trap_exit, true),
	Port = open_port({spawn, ExtPrg}, [in, exit_status, stream, {line, 255}]),
	loop(Port).
	
loop(Port) ->
	receive
		{Port, Data} ->
			{_,{_,Chunk}} = Data, % strip out unnecessary data
			Tokens = string:tokens(Chunk, " "),
			[Mac, SeqNo, SignalStrength] = Tokens,
			% io:format("Mac Address: ~s Sig Strength: ~s SeqNo: ~s~n", [Mac, SignalStrength, SeqNo]),
			ServerNode = 'dumbledore@Kevins-MacBook-Pro-3',
			rpc:call(ServerNode, servernode, store, [{node(), Mac, list_to_integer(SignalStrength), list_to_integer(SeqNo)}]),
			loop(Port);
		stop ->
			Port ! {self(), close},
			receive
				{Port, closed} ->
					exit(normal)
				end;
			{'EXIT', Port, Reason} ->
				exit(port_terminated)
	end.