# Software Architecture
#### Project 2, Marauder's Map
#### Team Easy, CIS 422, Fall 2013

## Architecture Goals (Required Qualities)
The brief listing here is provided as a review. See the SRS for detailed specifications.
### Behavioral Qualities
* Performance - the graphical interface should run smoothly (without visible stuttering due to CPU) on recent smartphone browsers. Back-end server and capture nodes should be able to run on commodity laptops (within the last 2 years) that have compatible Wi-Fi cards.
* Security - detected MAC addresses should not be available to end-users. Users should not be able to take control of the server.
* Usability - when the system is running, all it should take is a visit to the map's webpage for the map to function and display locations.

### Developmental Qualities
* Modifiability - modules should be able to be modified independently if the change does not involve modifying the generic data format used to exchange information between modules.
* Portability - preferably, the program will not use proprietary frameworks that are tied to one operating system.
* Understandability - it should be clear which module is responsible for what, how modules are related, and which parts would need to be changed to implement new features.
* Independent work assignments - multiple modules should be able to be developed independently based on agreed interfaces.

## Design Decisions
### Constraints & Assumptions
* The algorithm is likely to change often. A switch from trilateration to fingerprinting, and back, should be easy to make. Many changes are needed to adjust the algorithm to adapt to real-world data. Such changes to the algorithm should not require any more than a single setting change in the server at most.
* Some languages are better suited than others for some tasks, and a diverse system may require several different languages. HTML, CSS, and JavaScript are the only options for a cross-platform system that can be instantly accessed by guests. Erlang is strong with coordinating networked systems and maintaining uptime. Python has libraries that excel at scientific computing and machine learning. The only way we can meet our desired requirements is to use languages for their strengths.
* Development should be able to proceed in parallel, and distributed among several developers, not only to shorten the critical path, but to better manage risk.

### Design Rationale
Based on the constraints and assumptions, to address the design goals, we chose to divide the program into three main modules: front-end client, back-end server/capture nodes, and back-end algorithms. Each module corresponds to a set of languages: the front-end is HTML/CSS/JS, the nodes are in Erlang, and the algorithms are in Python. Dividing the program along these lines follows logically from our constraints and assumptions:

1. The algorithms module is separate from the controller, and two steps removed from the client. Neither the client nor the server know anything about the algorithm implementation, which allows it to change frequently while hiding details behind a clean interface.
2. Each module uses its language's strengths to achieve a combined system. HTML/CSS/JS allows the front-end to meet its goals of easy usability. Erlang makes the server and capture nodes easier to write because Erlang manages node coordination and networking automatically. Python is a boon to algorithm development for its robust data analysis and machine learning libraries.
3. The controller can be developed by our one developer who has experience with Erlang, the algorithms by our one developer who has experience with machine learning, and the front-end by our developers that have experience with JavaScript. This allows all three components to proceed in parallel.
    
Furthermore, this choice of modular architecture is an excellent fit for our design goals:

* Performance - off-loading the processing to the server-side allows the front-end client to run smoothly on smartphones. In addition, only the server requires any measure of computing power to run the location algorithm - the capture nodes only send data and can run on weak CPUs.
* Security - MAC addresses are handled server-side and never sent to the front-end client. Separation of data capture, data analysis, and node coordination allows future implementation of sandboxing and easy privilege separation.
* Usability - a standards-compliant webpage GUI lets anyone start using the map right away.
* Modifiability - the main dependencies between the front-end client, back-end server/capture nodes, and algorithms are the data format used to exchange information, so they are easily modified independently, without having to know implementation details.
* Portability - HTML, CSS, JavaScript, Erlang, and Python are all cross-platform languages.
* Understandability - there is be a clear division between the responsibilities of the front-end, the server, and the algorithms, and a clear relationship between them. Each component is clearly defined so that new features or changes would occur in a consistent and organized manner.
* Independent work assignments - front-end client, back-end server and capture nodes, and back-end algorithms should be able to be developed independently based on agreed interfaces.

## Architectural Structures

### Module Structure
#### Components
Starting with front-end and back-end, the program can be further divided into more detailed submodules. The front-end is small enough that it is one submodule. The back-end consists of around three submodules: server, capture nodes, and algorithms. The reasoning behind this modularization is explained above in the design rationale.

#### Relations & Interfaces
##### Front-end client:
###### Services:
Display locations on a map for the user. These visualizations may be animated.
###### Secrets:
The visual design and display of locations, how locations are animated, etc.

##### Server:
###### Services:
Provide location data to front-end clients. Provide signal strengths for algorithm to analyze.
###### Secrets:
How to communicate with capture nodes. How to prepare data for analysis and for the client.

##### Capture nodes:
###### Services:
Provide a labeled stream of signal strength data to the server.
###### Secrets:
How the data is captured, e.g., which program is used and with what parameters.

##### Algorithm:
###### Services:
Estimate a location when given a set of signal strengths.
###### Secrets:
The algorithm and implementation used to perform estimation.

### Data Flow Structure

Each capitalized group of text (ex. "Server") represents a component.
Each arrow indicates a "sends-data-to" relationship.
The label on each arrow specifies the data type used in the interface.


                                             *:1          1:1
    Front-end client  <---estimated locations---  Server  ----signal strengths--->  Algorithm
                                                     ^    <--estimated locations--
                                                     |
                                                     signal strengths
                                                     | 3:1
                                                  Capture
                                                   nodes
                                               
                                               
Although the front-end client initially connects to the server to receive data, the main flow of data is as follows:

1. Signal strengths are recorded by the capture nodes and sent to the server
2. When a packet has signal strength measurements from all of the capture nodes, the server combines them into an ordered tuple and sends it to the algorithm.
3. The algorithm outputs an estimated location, which is read by the server.
4. The server pushes new locations over a persistent socket to the front-end client.

