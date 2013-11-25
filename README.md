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

The server node should run fine for testing, but if you want to run the server node and make it accessible from a network, run this:

	./startserver.sh

which will build, change the host to the detected IP address reported by `ifconfig`, then run the server.

Setup (Capture Nodes)
---------------------
Building and running the capture nodes is done differently from the server nodes.
In the root of the Marauder's Map project, edit the file `mm_capture.src`:

	{mm-receiver, 'mm@localhost'}.

Change `localhost` to an ip address of the server.

Run `ifconfig` to see what is the identifier of your wireless card, then change `en0` to whatever ifconfig says is your wireless interface:

	{interface, <<"en0">>}.

After changing the settings, run the start script, where `nodeA` is whichever node you were assigned to run (nodeA, nodeB, or nodeC):

	./startnode nodeA

This starts an erlang node with all the settings filled in correctly.