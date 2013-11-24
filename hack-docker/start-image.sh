#!/bin/bash

if (( $# < 2 ))
then
	echo Usage: $0 SERVERIMAGE PORTNUMBER
	exit 1
fi

SERVERIMAGE=$1
PORTNUMBER=$2
WEBWORKER=$(sudo docker run -d -p $PORTNUMBER $SERVERIMAGE /usr/local/bin/runapp | cut -c -12)
echo Instance $WEBWORKER
WEBPORT=$(sudo docker port $WEBWORKER $PORTNUMBER | awk -F: '{ print $2 }')
echo is running on host port $WEBPORT
