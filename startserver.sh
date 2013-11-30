#!/bin/bash
echo MM Server Start script
make
MYIP=`ifconfig | grep 'inet ' | grep -v '127.0.0.1' | awk '{ print $2}'`
echo Your IP address is: $MYIP
sed -i "s/127.0.0.1/$MYIP/" ./_rel/releases/0.1/vm.args
echo -e "\n-mnesia dir '\"`pwd`/mnesia\"'" >> ./_rel/releases/0.1/vm.args
./_rel/bin/mm console