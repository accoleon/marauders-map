Marauder's Map
==============

Prerequisites
-------------
**Hardware Requirements**

* Minimum of 3 computers (4 computers recommended)
* Computers must have WiFi B/G/N network interface cards compatible with
	wireshark ([recommended wireless cards](http://www.aircrack-ng.org/doku.php?id=faq#what_is_the_best_wireless_card_to_buy))

**Software Requirements**

* Computers must be running updated versions of OSX 10.9 or Linux Kernel 3.5
* [Erlang R16B02](https://www.erlang-solutions.com/downloads/download-erlang-otp) 
	must be installed and in the `$PATH`
* [Wireshark 1.10.3](http://www.wireshark.org/download.html) must be installed
* [GNU make 3.5](http://www.gnu.org/software/make/) and above must be installed
* [GNU wget](http://www.gnu.org/software/wget/) must be installed for `erlang.mk`
	to function 
* [Git 1.7](http://git-scm.com/downloads) and above must be installed and in the
	$PATH

Setting Up Your Server
----------------------
To build the Marauder's Map server, run:

	make

To start the server node in the foreground:

	$./_rel/bin/mm console

To start the server in the background:

	$./_rel/bin/mm start

After starting the server in the background, the server will run as a background
process. To view the server console, run:

	$./_rel/bin/mm attach

Pointing the browser to http://localhost:8080 to bring up Marauder's Map.

The server node should run fine for testing, but if you want to run the server
node and make it accessible from a network, run this:

	./startserver.sh

which will build, change the host to the detected IP address reported by 
`ifconfig`, then run the server.

Setting Up Your Capture Nodes
-----------------------------
Building and running the capture nodes is done differently from the server nodes.
In the root of the Marauder's Map project, edit the file `mm_capture.src`:

	{mm-receiver, 'mm@localhost'}.

Change `localhost` to an ip address of the server.

Run `ifconfig` to see what is the identifier of your wireless card, then change `en0` to whatever ifconfig says is your wireless interface:

	{interface, <<"en0">>}.

After changing the settings, run the start script, where `nodeA` is whichever node you were assigned to run (nodeA, nodeB, or nodeC):

	./startnode nodeA

This starts an erlang node with all the settings filled in correctly.

FAQ/Misc.
---------------------