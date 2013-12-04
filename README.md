Marauder's Map
==============

Prerequisites
-------------

### Hardware Requirements 

* Minimum of 3 computers (4 computers recommended)
* Computers must have 802.11 b/g/n network interface (Wi-Fi) cards compatible with
	Wireshark ([recommended wireless cards](http://www.aircrack-ng.org/doku.php?id=faq#what_is_the_best_wireless_card_to_buy))

### Software Requirements

#### Server and capture nodes
* Computers running updated versions of OS X 10.9+ or Linux Kernel 3.5
* [Erlang R16B02](https://www.erlang-solutions.com/downloads/download-erlang-otp)
* [GNU make 3.5](http://www.gnu.org/software/make/) or above
* [GNU wget](http://www.gnu.org/software/wget/) (required for `erlang.mk`)
* [Git 1.7](http://git-scm.com/downloads)

#### Capture nodes only
* [Wireshark 1.10.3](http://www.wireshark.org/download.html)

#### Server only
* [Python 2.7](http://www.python.org/download/releases/2.7.6/) (**not** version 3)
* [scikit-learn 0.14](http://scikit-learn.org/stable/install.html)
* [numpy](http://www.numpy.org)
* [pandas](http://pandas.pydata.org)

The above software components must all be installed and accessible in the `$PATH`. Some installation instructions are included below.


Installing the Prerequisites
----------------------------

### OS X 10.9

Installing Xcode command-line developer tools:

	xcode-select --install

Installing Homebrew:

	ruby -e "$(curl -fsSL https://raw.github.com/mxcl/homebrew/go)"

Add the following line to your `~/.bashrc`:

	export PATH="$(brew --prefix coreutils)/libexec/gnubin:/usr/local/bin:$PATH"    

Installing GNU make:

	brew install make

Installing GNU wget:

	brew install wget

Installing Python 2.7:
    
    brew install python

and add the following to your `~/.bashrc`:

    export PATH=/usr/local/share/python:$PATH

Run the following to install pandas and scikit-learn (which should also pull in numpy as a dependency):

    pip install scikit-learn pandas

### Ubuntu Linux 13.10

Installing development tools:

	sudo aptitude install build-essential

Installing wireshark:

	sudo aptitude install wireshark

The [Erlang Solutions page](https://www.erlang-solutions.com/downloads/download-erlang-otp) has instructions for installing Erlang R16B02 on Ubuntu.

Installing Python and pip:

    sudo aptitude install python python-pip

Using pip to install the Python packages:

    pip install scikit-learn pandas

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

### Viewing the map
On the server, point a browser to `http://localhost:8080` to bring up Marauder's Map.

The server node should run fine for testing, but if you want to run the server
node and make it accessible from a network, run this:

	./startserver.sh

which will build, change the host to the detected IP address reported by 
`ifconfig`, then run the server.

Setting Up Your Capture Nodes
-----------------------------

Building and running the capture nodes is done differently from the server node.
In the root of the Marauder's Map project, edit the file `mm_capture.src`:

	{mm-receiver, 'mm@localhost'}.

Change `localhost` to the IP address or hostname of the server.

Run `ifconfig` to determine the interface name of your wireless card, then change `en0` to whatever ifconfig says is your wireless interface:

	{interface, <<"en0">>}.

After changing the settings, run the start script, where `nodeA` is whichever node you were assigned to run (nodeA, nodeB, or nodeC):

	./startnode nodeA

This starts a capture node with all the settings filled in correctly.

### Important notes on Wi-Fi capture
Make sure that all capture nodes are on the same wireless channel, and are able to communicate with the server node. Usually the best wireless channel to capture on is the same as the dominant wireless network in the area. Note that if you are unable to join this network and use it to connect the nodes, a short-range option is to create an ad-hoc network with the same channel.

FAQ
---

#### `make` gives me an error!
Be sure to have `wget`, Erlang, GNU `make` installed and available in your `$PATH`.

#### My capture nodes are not capturing any wireless data!
Be sure to have a compatible wireless interface card that supports libpcap
capturing in monitor mode. Read the [short guide](http://www.aircrack-ng.org/doku.php?id=faq#what_is_the_best_wireless_card_to_buy)
for compatible wireless cards.

#### My capture nodes are not communicating with my server!
Be sure that your server and all your capture nodes are on the same local area
network and subnet, and can ping each other. Check your firewall, and open port
`4369` for the Erlang Port Mapper Daemon (epmd) to communicate.

To troubleshoot connection problems, enter `nodes().` in the server's Erlang console to see the list of connected nodes.

#### I can't see Marauder's Map on my web browser!
Be sure that your firewall has not blocked port `8080`, and that the server is 
running without errors.

#### Marauder's Map looks strange/frozen on my web browser
Marauder's Map uses SVG and WebSocket, and requires a modern browser. It has been tested to work with Chrome v31 and Safari 7.0.