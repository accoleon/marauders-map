#!/bin/bash
NODE=$1
echo MM $NODE Start script
MYIP=`ifconfig | grep 'inet ' | grep -v '127.0.0.1' | awk '{ print $2}'`
echo Your IP address is: $MYIP
cat mm_capture.src | sed "s/{cap, test}/{cap, $NODE}/" > src/mm_capture.settings
cd src
erlc mm_capture.erl mm_misc.erl
erl -name $NODE@$MYIP -setcookie mm -s mm_capture