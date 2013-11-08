%% this is a records header file

% hash comes from erlang:phash2({mac, seqno})
% nodeA, nodeB, nodeC denotes the signal strengths from the respective capturenodes
% lastupdated is a timestamp in seconds since 1970

%% needs more accuracy in order to make use of multilateration - each node has its own transmitted time so we can calculate TDOA (Time Difference of Arrival)
-record(row, {hash, mac, nodeA, nodeATime, nodeB, nodeBTime, nodeC, nodeCTime, lastupdated}).