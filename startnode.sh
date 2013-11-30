#!/bin/bash

usage() {
	echo "usage: ./startnode.sh nodeA | nodeB | nodeC";
}

if [ $# -ne 1 ]; then
	usage
	exit 1
elif [ $1 != "nodeA" ] && [ $1 != "nodeB" ] && [ $1 != "nodeC" ]; then
	echo "./startnode.sh: illegal argument $1"
	usage
	exit 1
else
	NODE=$1
	echo MM $NODE Start script
	MYIP=`ifconfig | grep 'inet ' | grep -v '127.0.0.1' | awk '{ print $2}'`
	echo Your IP address is: $MYIP
	cat mm_capture.src | sed "s/{cap, test}/{cap, $NODE}/" > src/mm_capture.settings
	cd src
	erlc mm_capture.erl mm_misc.erl
	erl -name $NODE@$MYIP -setcookie mm -s mm_capture
fi
