%% this is a records header file
-record(rawrow, {mac, ss, seqno, fromnode, lastupdated}).

% hash comes from erlang:phash2({mac, seqno})
-record(row, {hash, nodeA, nodeB, nodeC, lastupdated}).