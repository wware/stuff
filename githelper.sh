#!/bin/bash

if [ "X" == "X$1" ]
then
    git show HEAD | grep '^diff' | sed 's#.*b/##'
elif [ "Xid" == "X$1" ]
then
    echo -n "Change-Id: I"
    for i in  {1..10}
    do
        printf "%04x" $((RANDOM % 65536))
    done
    echo
else
    if [ "X" == "X${GITGOAL}" ]
    then
        echo Please set GITGOAL environment variable.
        echo It should identify the commit you\'re moving toward.
    fi
    for x in $@
    do
	echo "git show ${GITGOAL}:$x > /tmp/zz"
	git show ${GITGOAL}:$x > /tmp/zz
	echo "meld /tmp/zz $x"
	meld /tmp/zz $x
    done
fi
