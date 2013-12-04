# Software Architecture Document
#####Marauder Map Application v1.1
#####Team Easy, CIS 422, Fall 2013

####Table of Contents

1.	[Introduction](#introduction)
   1.	[Purpose](#purpose)
   2.   [Scope](#scope)
   3.   [Definitions, Acronyms, and Abbreviations](#definitions)
2.	[Architectural Representation](#representation)
3.	[Architectural Goals and Constraints](#goals)
   1.   [Privacy](#privacy)
   2.	[Accuracy](#accuracy)
   3.	[System Flexibility](#flexibility)
   4.   [Performance](#performance)
4.   [Logical View](#logical)
5.   [Deployment View](#deployment)
6.   [Implementation View](#implementation)
7.   [Data View](#data)

##1. <a name="introduction">Introduction</a>

###1.1 <a name="purpose">Purpose</a>
The intent of this document is to convey the architectural decisions that have been made for the "Marauder's Map" application. Included is an overview of the system, and several different views to show the different aspects of the application.

###1.2 <a name="scope">Scope</a>
Contained in this document is information relating to the overall architectural design of the "Marauder's Map" application, including the structure of the data capture node, data processing server, coordinate calculation, and display interface.

###1.3 <a name="definitions">Definitions, Acronyms, and Abbreviations</a>
####Erlang
A general-purpose concurrent, garbage-collected programming language and runtime system. The sequential subset of Erlang is a functional language, with eager evaluation, single assignment , and dynamic typing. It was designed by Ericsson to support distributed, fault-tolerant, soft-real-time, non-stop applications. It supports hot swapping, so that code can be changed without stopping a system. The "Marauder's Map" uses it for the capture nodes and data processing server. 

####Triangulation/Trilateration
The process of determining absolute or relative locations of points by measurement of distances, using the geometry of circles, spheres or triangles.

####Fingerprinting
The process of creating a many-to-one mapping of multiple test data points into one real data point. For our purposes, we calibrated our algorithm by standing in various positions around the map and saying "This is where we are now. Data from this point looks like X, so anything that looks like X in the future is from this point."

##2. <a name="representation">Architectural Representation</a>
To provide a deeper understanding into the design of the Marauder Map application, the following sections are organized around several different architectural views of the system: 

   -   The Logical view will provide an overview of the components and subsystems.
   -   The Deployment view describes how the application will be deployed.
   -   The Implementation view goes into more detail of the objects and subsystems of the application.
   -   The Data view will show in what way the data from the application is stored, organized, and accessed.

##3. <a name="goals">Architectural Goals and Constraints</a>

###3.1 <a name="privacy">Privacy</a>
- Collected data must not be stored in any permanent storage. The system must use data as it is collected, and then immediately discard it.
- The system must not make use of any data that can be use for identification purposes, or use any data that is not readily available via public analysis. 

###3.2 <a name="accuracy">Accuracy</a>
- Accuracy of data and calculation algorithm is a must. The system must be able
	to display device locations on the map with an error of no more than 10
	meters. 
- Device locations must be reflected on the map in near real time with a
	refresh rate of 500 milliseconds or less.

###3.3 <a name="flexibility">System Flexibility</a>
- This project must be modifiable and flexible for future design goals, such
	as: integration with larger displays, addition of more capture nodes to
	increase precision, or the ability to identify a device as being owned by
	a user if they so choose.
- All modules must be designed in isolation, such that changing one module
	should not propagate it's changes to the other modules requiring additional
	work. 

- The display interface must be designed such that it will be possible to
	display the map on different operating systems and devices (Mac OSX,
	Windows, Android…)
	
###3.4 <a name="performance">Performance</a>
- System must be able to collect a large amount of data and display multiple
	different device locations on the map in near real time (less than 500 
	milliseconds).
- System must be reliable enough to provide over 90% uptime in a typical server
	environment of 40 hours per week.
	
##4. <a name="logical">Logical View</a>

Marauder's Map is divided into two large sections: *Back-End* and *Front-End*

#### Back-End
The Back-End is non-user facing and consists of nine major components: Receiver,
Analyzer, Python Signal Analyzer, WebSocket Server, Application Supervisor, Mnesia,
gproc, jsx, and a set of Capture Nodes.

##### Receiver
The Receiver listens for incoming Wifi Signal data from a set of Capture Nodes
and tabulates the signal strengths for each particular WiFi packet. Upon 
collecting a "complete" packet (one that consists of at least 3 signal strengths),
it sends the completed packet to the Analyzer for analysis.

##### Analyzer
The Analyzer consists of two subcomponents: trainer and analyzer. It parses the
packets sent by the Receiver and divides them into two broad classes: a general
packet (sent by a generic device by a member of the general public) and a trainer
packet (sent by a set of specific MAC addresses belonging to the devices of
the developers of Marauder's Map).

General packets are sent to the Python Signal Analyzer to be analyzed. It expects
a return of x, y coordinate data. When the Python Signal Analyzer returns
coordinate data, the Analyzer will broadcast the location data to all connected
Map clients.

Trainer packets are stored in the mm_training table of the Mnesia database. Once
collated, the training data can be dumped into a text file to serve as calibration
data for the Python Signal Analyzer.

##### Python Signal Analyzer
The Python Signal Analyzer calibrates itself with training data from the Analyzer
and performs matrix analysis to find the closest coordinates that match the wireless
signature in a packet.

##### WebSocket Server
The WebSocket Server handles all WebSocket requests from the Front-End clients:
Map and Trainer tool. It acts as the intermediary between the Erlang Back-End and
the Javascript/HTML Front-End clients.

Every WebSocket process registers itself as a subscriber of a pubsub (Publisher-Subscriber)
gproc (global process manager) in order to receive broadcasts of location data
published by the Analyzer.

Data is packed into JSON objects which conform to an event-handling protocol that
is recognized by the Front-End clients. The jsx library is used for the encoding
and decoding of JSON data to Erlang terms.

##### Application Supervisor
The Application Supervisor is an Erlang OTP (Open Telecom Platform) behavior that
monitors the Analyzer, WebSocket Server, Receiver, Mnesia, gproc, and jsx. It
ensures uptime on the components and restarts those components in the event of
failure and errors are logged for administrative purposes.

By conforming to the Erlang OTP specifications, Marauder's Map can be run on the
OTP platform, and is supplemented with high uptime and reliability, comprehensive error
logging and robust failover facilities. 

##### Mnesia
Mnesia is a distributed database that is developed for use within the Erlang OTP.
It provides distributed table-based database capabilities and can function in
memory-only, disk-persisted, and a combination of both modes.

The Mnesia database runs as a separate process and maintains the `mm_training` and
`mm_trained_coord` tables. `mm_training` stores in tuple form the x,y coordinates and
signal strengths of those coordinates for use as calibration data in the Python 
Signal Analyzer. `mm_trained_coord` stores trained coordinates to prevent
duplication of data for identical coordinates.

##### gproc
gproc is a global process manager that encapsulates messaging between processes
in a more flexible form than Erlang's innate message passing abilities. It allows
for easy implementation of the PubSub protocol (Publisher-Subscriber) that
the Analyzer uses to broadcast location data to all connected WebSocket clients.

By abstracting this functionality to a third-party library, development and
maintainance requirements are reduced and we can expect a good level of reliability
from the component.

##### jsx
jsx is an Erlang-JSON encoder/decoder library. Marauder's Map uses JSON objects
to encapsulate event data and location data as the main form of communication between
the Back-End and the Front-End.

JSON is a widely-used and highly portable specification that allows for reduced 
development time in both the Back-End encoding components and Front-Ent decoding
functions. By using the third-party jsx library, one can ensure code reliability
and reduced development pressure.

##### Capture Nodes
The Capture Nodes is a set of computers which run the capture node portion of 
Marauder's Map. Each node is an Erlang Node (a virtual machine) that runs
tshark, the command-line component of Wireshark, to sniff wireless data from the air.

The Capture Nodes encapsulate the sniffing behavior provided by tshark and adds
blacklisting and whitelisting capabilities to filter out unwanted data. This ensures
no privacy violations occur from the general public not wanting their data to be
tracked by Marauder's Map.

#### Front-End
The Front-End is user facing and consists of two major components: Map and
Trainer.

##### Map 
Accessed through a web browser on a large, public, flatscreen display, it
receives data from the Back-End in near-realtime over WebSockets and utilizes
Javascript ([D3.js](http://www.d3js.org)) to animate and display location data on the screen.

##### Trainer
Accessed through a modern web browser on a computer or mobile device, this tool
provides the administrator an interface to collect WiFi fingerprinting training
data.

##5. <a name="deployment">Deployment View</a>
A minimum of 3 computers are required for deployment of Marauder's Map. Ideally,
4 computers are required for maximum functionality and reduced troubleshooting.
When using 3-computer deployment, the Server Node will host a Capture Node on top
of its usual set of services.

### The Server Node
1 computer will host the Server Node. Its Erlang OTP environment will house the 
bulk of the Marauder's Map functionality. The following services are run on
the Server Node:

* Receiver - Receives data from Capture Nodes
* Analyzer - Sorts data from Receiver to be trained or signal-analyzed by Python
* Python Signal Analyzer - Analyzes signal data and calibrates itself with data from Analyzer
* Web Server - Serves HTTP to Front-End components
* WebSocket Server - Provides interface between Front-End and Back-End components
* Mnesia - Persists training data from Analyzer
* Application Supervisor - Monitors all components on the Server Node
* gproc - Provides PubSub functionality for data broadcasts
* jsx - Encodes and Decodes JSON data

### The Capture Nodes
3 computers will each host a Capture Node. The Capture Nodes will each host an
Erlang environment that will capture wireless data using tshark and package it
for analysis to the Analyzer. The following services are run on the Capture Nodes:

* Capture - Captures output from tshark and packages it for consumption on the Analyzer
* tshark - Actual capturing of WiFi packets, with configuration and black/white listing functionality by Capture


## 6. <a name="implementation">Implementation View</a>
The Git repository is organized this way:

Marauder's Map

* `_rel` - 	contains a *release* packaged by the maketool It contains all the files required to run Marauder's Map  
* `algo` - 	Python Signal Analyzer code
* `deps` - 	dependencies required by the project. Automatically acquired and built by the maketool
* `doc` - 	Erlang documentation generated by the edocs tool. Contains module/type specification documents used in project
* `ebin` - 	compiled Erlang code. Generated by maketool
* `mnesia` - 	disk files for Mnesia database. Maintained by Mnesia database
* `priv` - 	Front-End components. HTML documents that are housed along with other static data
	* `static` - directory holding static data such as javascript libraries and css style files
	* `map.html` - Map Front-End component
	* `trainer.html` - Trainer tool Front-End component
* `src` - 	Erlang source code files
	* `mm_analyzer.erl` - Analyzer source file
	* `mm_app.erl` - Webserver and Application source file
	* `mm_capture.erl` - Capture Node source file
	* `mm_misc.erl`- Miscellaneous library functions source file
	* `mm_receiver.erl` - Receiver source file
	* `mm_records.hrl` - Records header file, containing definitions for Erlang records used in the project
	* `mm_sup.erl` - Application Supervisor source file
	* `mm_ws_handler.erl` - WebSocket Server source file
	* `mm.app.src` - Erlang Application Definition file. Used for packaging the project into a usable OTP release package
* `ARCHITECTURE.md` - Architecture documentation in markdown format
* `erlang.mk` - Erlang maketool
* `Makefile` - Makefile for the project
* `mm_capture.src` - configuration file for capture nodes, used by the start script for capture nodes
* `README.md` - User/Administrator documentation in markdown format
* `relx` - Erlang Releases package tool
* `relx.config` - Configuration file for the relx tool
* `startnode.sh` - start script for capture nodes. Reads in mm_capture.src and autodetects IP addresses and settings and runs the capture node
* `startserver.sh` - start script for the server node. Autodetects settings and launches the server
* `trainingdata.txt` - Gathered training data in text form

## 7. <a name="data">Data View</a>
The following components deal with data:

### Capture Node -> Receiver
Captured wireless packet data from tshark in the form of

	{ThisNode, {MAC, SignalStrength, SeqNo}}

where `ThisNode` is the unique identifier of the particular capture node, `MAC`
is the source MAC address, `SignalStrength` is the signal strength, `SeqNo` is
the sequence number of the wireless packet.

### Receiver -> Analyzer
Tabulated wireless packet data is sent from the Receiver to the Analyzer in the
form of `#row` records (defined in `mm_records.hrl`).

### Analyzer -> Python Signal Analyzer
The Analyzer sorts non-training wireless packets and sends them to the Python
Signal Analyzer using stdout:

	MACAddress nodeA nodeB nodeC

where `MACAddress` is the source MAC address of the wireless packet, and
`nodeA`/`nodeB`/`nodeC` are integers representing the signal strengths captured
from their respective capture nodes.

### Analyzer -> Mnesia
The Analyzer sorts training wireless packets and stores them in the Mnesia 
database in the form of `#mm_training` and `#mm_trained_coord` records.

### Analyzer -> WebSocket Server
The Analyzer will broadcast location data to all connected WebSocket clients
using the PubSub protocol provided by gproc in the form of:

	-define (WS_KEY, {pubsub, ws_broadcast}).

`WS_KEY` is used as the broadcast key (which is the same key used by WebSocket 
processes that are subscribed to PubSub). No identifying data is sent.

The Analyzer will send training data acknowledgment messages to the WebSocket
Server over PubSub using `WS_KEY`. No identifying data is sent.

The Analyzer will provide a list of trainer devices used for debugging the project
in the `get_trainers()` function.

### WebSocket Server -> Map
Upon receiving a broadcasted location data, the WebSocket Server will encode that
data in JSON and send it to the Map in the form of:

	[{event: "position"}, {data: {position: [
		i: 12345, // a numeric hash of the MAC address created by the erlang:phash2 function
		devicetype: "trainer" or "general", // representing whether the wireless packet belonged to a member of the public or a trainer
		name: "name", // a string, for trainers it holds the registered name, for general users it holds the hash of i in string form
		x: 12, // a float, representing the x coordinate of the device
		y: 12, // a float, representing the y coordinate of the device
	]}}]

### WebSocket Server -> Training tool
The WebSocket Server reacts to the following events from the Training tool:

`get_trainers`: Gets a list of trainers. Calls `mm_analyzer:get_trainers()`
`get_trained_coords`: Gets a list of trained coordinates. Calls `mm_analyzer:get_trained_coords()`
`start_training`: Starts training on a specified coordinate. Calls `mm_analyzer:start_training(Trainer, X, Y)`
`end_training`: Ends training on a specified coordinate. Calls `mm_analyzer:end_traiing(Trainer, X, Y)`

All events are encoded in JSON in the following form:

	[{event: "event"}, {data: "data"}]

Where "event" is the event name, and "data" consists of whatever data is associated with
that particular event.