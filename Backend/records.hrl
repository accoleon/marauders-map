ø%% this is a records header file
-record(rawrow, {mac, ss, seqno, fromnode, lastupdated}).

% hash comes from erlang:phash2({mac, seqno})
% nodeA, nodeB, nodeC denotes the signal strengths from the respective capturenodes
% lastupdated is a timestamp in seconds since 1970
-record(row, {hash, nodeA, nodeB, nodeC, lastupdated}).