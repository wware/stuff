#!/bin/bash

GOAL=goal
#GOAL=fixes-2

if [ "X" == "X$1" ]
then
    git show HEAD | grep '^diff' | sed 's#.*b/##'
else
    for x in $@
    do
	echo "git show ${GOAL}:$x > /tmp/zz"
	git show ${GOAL}:$x > /tmp/zz
	echo "meld /tmp/zz $x"
	meld /tmp/zz $x
    done
fi
