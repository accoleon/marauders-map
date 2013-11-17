Marauder's Map
==============

Prerequisites
-------------
`erlang.mk` requires GNU make and expects to be ran in a standard unix environment with Erlang installed and in the `$PATH`.
`erlang.mk` uses `wget` for downloading the package index file when the `pkg://` scheme is used.
`git` is in the `$PATH`.

Setup (Server Node)
--------------------
To build Marauder's Map, run the following command:

	make

To start the server node in the foreground:

	$./_rel/bin/mm console

Then point your browser to http://localhost:8080

The server node should run fine for testing, but if you want to run the server node and make it accessible from a network, you need to edit the file `_rel/releases/1/vm.args` and change the line:

	-name mm@localhost

from `@localhost` to `@xxx.xxx.xxx.xxx` where xxx.xxx.xxx.xxx is the ip address of the server.

Setup (Capture Nodes)
---------------------
Building and running the capture nodes is done differently from the server nodes.
Enter the `src` directory in the root of the Marauder's Map project, then edit the file `mm_capture.settings`:

	{mm-receiver, 'mm@localhost'}.

Change `localhost` to an ip address of the server.

Additionally, the tshark line may need to be changed depending on your wifi card, thus run `ifconfig` to see what is the identifier of your wireless card, and add `-i wlan1` right after `sudo tshark `. For examplem if `ifconfig` shows your wireless interface to be `wlan0`, the tshark line would look like:

	{tshark, "sudo tshark -i wlan0 -f \"wlan[0] != 0x80\" -Y \"wlan.sa && wlan.seq && radiotap.dbm_antsignal\" -I -N m -Tfields -E separator=/s -e      wlan.sa -e wlan.seq -e radiotap.dbm_antsignal -l"}.

After changing the settings, run the erlang shell by running:

	sudo erl -name xxx@127.0.0.1 -setcookie mm

This starts an erlang node with the name xxx (change it to a unique name, since we have 3 capture nodes, they must not conflict with each other). `-setcookie mm` sets the "magic cookie" of the erlang vm to match that of the server node (which is given the cookie `mm` by default). `sudo` is required because the erlang vm will need sudo permissions to start tshark in monitor mode.

Once in the erlang shell, run:

	c(mm_capture).
	c(mm_misc).
	mm_capture:start(nodeA).	

Where nodeA can be nodeB, or nodeC (depending on the node you are starting). If you just ran `mm_capture:start().` With no arguments, it will default to `nodeA`.