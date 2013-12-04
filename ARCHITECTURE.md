# Software Architecture Document
#####Marauder Map Application v1.1
#####Team Easy, CIS 422, Fall 2013

####Table of Contents

1.	[Introduction](#introduction)
   1.	Purpose
   2.   Scope
   3.   Definitions, Acronyms, and Abbreviations
2.	Architectural Representation
3.	Architectural Goals and Constraints
   1.   Privacy
   2.	Accuracy
   3.	System Flexibility
   4.   Performance
4.   Logical View
5.   Deployment View
6.   Implementation View
7.   Data View

##<a name="introduction">1. Introduction</a>

1.1 Purpose
The intent of this document is to convey the architectural decisions that have been made for the "Marauder's Map" application. Included is an overview of the system, and several different views to show the different aspects of the application.


1.2 Scope
Contained in this document is information relating to the overall architectural design of the "Marauder's Map" application, including the structure of the data capture node, data processing server, coordinate calculation, and display interface.


1.3 Definitions, Acronyms, and Abbreviations
Erlang - a general-purpose concurrent, garbage-collected programming language and runtime system. The sequential subset of Erlang is a functional language, with eager evaluation, single assignment , and dynamic typing. It was designed by Ericsson to support distributed, fault-tolerant, soft-real-time, non-stop applications. It supports hot swapping, so that code can be changed without stopping a system. The "Marauder's Map" uses it for the capture nodes and data processing server. 



Triangulation/Trilateration - is the process of determining absolute or relative locations of points by measurement of distances, using the geometry of circles, spheres or triangles.



Fingerprinting - is the process of creating a many-to-one mapping of multiple test data points into one real data point. For our purposes, we calibrated our algorithm by standing in various positions around the map and saying "This is where we are now. Data from this point looks like X, so anything that looks like X in the future is from this point."



2. Architectural Representation
To provide a deeper understanding into the design of the Marauder Map application, the following sections are organized around several different architectural views of the system: 

   -   The Logical view will provide an overview of the components and subsystems.

   -   The Deployment view describes how the application will be deployed.

   -   The Implementation view goes into more detail of the objects and subsystems of the application.

   -   The Data view will show in what way the data from the application is stored, organized, and accessed.



3. Design Goals


3.1 Privacy

- Collected data must not be stored in any permanent storage. The system must use data as it is collected, and then immediately discard it.

- The system must not make use of any data that can be use for identification purposes, or use any data that is not readily available via public analysis. 



3.2 Accuracy

- Accuracy of data and calculation algorithm is a must. The system must be able to display IDs on the map with a small amount of error margin (no more than 10 meters). 

- IDs must be able to display in real time with a refresh rate of 2 seconds or less.



3.3 System Flexibility

- This project must be modifiable and flexible for future design goals, such as: integration with larger displays, addition of more capture nodes to increase precision, or the ability to identify a device as being owned by a user if they so choose.

- All modules must be designed in isolation, such that changing one module should not propagate it's changes to the other modules requiring additional work. 

- The display interface must be designed such that it will be possible to display the map on different operating systems and device (Mac OSX, Windows, Android….)



3.4 Performance

- System must be able to collect a large amount of data and display multiple different IDs on the map without any significant delay (no more than 2 seconds) or crashes.



4. Logical View




5. Deployment View




6. Implementation View




7. Data View


