#!/bin/bash

NUM_DOCKERS=$1
URL=$2

for i in $(seq 0 $(($NUM_DOCKERS - 1)))
do
   PORT=$((80 + $i))
   sudo docker run -d -t -p $PORT:5000 wware/runapp /bin/runapp.sh $URL $i $NUM_DOCKERS
   #sudo docker run -i -t wware/runapp /bin/runapp.sh $URL $i $NUM_DOCKERS
done
