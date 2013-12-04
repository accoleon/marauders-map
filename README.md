Marauder's Map
==============

Prerequisites
-------------

### Hardware Requirements 

* Minimum of 3 computers (4 computers recommended)
* Computers must have WiFi B/G/N network interface cards compatible with
	wireshark ([recommended wireless cards](http://www.aircrack-ng.org/doku.php?id=faq#what_is_the_best_wireless_card_to_buy))

### Software Requirements

* Computers must be running updated versions of OSX 10.9 or Linux Kernel 3.5
* [Erlang R16B02](https://www.erlang-solutions.com/downloads/download-erlang-otp) 
	must be installed and in the `$PATH`
* [Wireshark 1.10.3](http://www.wireshark.org/download.html) must be installed
* [GNU make 3.5](http://www.gnu.org/software/make/) and above must be installed
* [GNU wget](http://www.gnu.org/software/wget/) must be installed for `erlang.mk`
	to function 
* [Git 1.7](http://git-scm.com/downloads) and above must be installed and in the
	$PATH

Installing the prerequisites
----------------------------

### OSX 10.9

Installing Xcode command line developer tools:

	xcode-select --install

Installing Homebrew:

	ruby -e "$(curl -fsSL https://raw.github.com/mxcl/homebrew/go)"

Add the following line to your `.bashrc`:

	export PATH="$(brew --prefix coreutils)/libexec/gnubin:/usr/local/bin:$PATH"

Installing GNU make:

	brew install make

Installing GNU wget:

	brew install wget

### Ubuntu Linux 13.10

Installing development tools:

	sudo aptitude install build-essential

Installing wireshark:

	sudo aptitude install wireshark

[Erlang Solutions Page](https://www.erlang-solutions.com/downloads/download-erlang-otp) has instructions to install Erlang R16B02 on Ubuntu

Checking Out The Project
------------------------

	git clone git://git.assembla.com/cis422f13_team3.maraudersmap.git
	cd cis422f13_team3.maraudersmap

You are now ready to build the project.

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

FAQ
---

**`make` gives me an error!**

Be sure to have `wget`, Erlang, GNU `make` installed and available in your `$PATH`.

**My capture nodes are not capturing any wireless data!**

Be sure to have a compatible wireless interface card that supports libpcap
capturing in monitor mode. Read the [short guide](http://www.aircrack-ng.org/doku.php?id=faq#what_is_the_best_wireless_card_to_buy)
to compatible wireless cards.

**My capture nodes are not communicating with my server!**

Be sure that your server and all your capture nodes are on the same local area
network and subnet, and can ping each other. Check your firewall, and open port
4369 for the Erlang Port Mapper Daemon (epmd) to communicate.

**I cannot see Marauder's Map on my web browser!**

Be sure that your firewall has not blocked port 8080, and that the server is 
running without errors.

**Marauder's Map looks strange/frozen on my web browser**

Marauder's Map currently only supports Chrome v31 and above, Safari 7.0 and
above.