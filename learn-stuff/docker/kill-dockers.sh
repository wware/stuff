#!/bin/bash

for x in $(sudo docker ps | tail -n +2 | cut -c -12)
do
    sudo docker stop $x
done
