#!/bin/sh

hidup=true
count=1

while $hidup
do
    echo $count
    if [ $count == 10 ];
    then
	echo "bye Gaffar"
	sleep 1
	shutdown -h now
    fi
    ((count++))
    sleep 1
done
