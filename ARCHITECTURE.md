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
The Back-End is non-user facing and consists of five major components: Receiver,
Analyzer, Python Signal Analyzer, WebSocket Server, Application Supervisor,
and a set of Capture Nodes.

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

##### Python Signal Analyzer

##### WebSocket Server

##### Application Supervisor

##### Capture Nodes

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




##6. <a name="implementation">Implementation View</a>




##7. <a name="data">Data View</a>


