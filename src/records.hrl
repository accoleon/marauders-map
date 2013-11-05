%% this is a records header file

% hash comes from erlang:phash2({mac, seqno})
% nodeA, nodeB, nodeC denotes the signal strengths from the respective capturenodes
% lastupdated is a timestamp in seconds since 1970
-record(row, {hash, mac, nodeA, nodeB, nodeC, lastupdated}).