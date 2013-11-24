#!/bin/sh

if [ $# -eq 0 ]
then
    cat << EOF
Usage: $0 TARBALL_URL [arg1] [arg2] ...
The tarball will be loaded into the Docker instance and unpacked. The run.sh script
(wherever it is in the tarball) will be run with subsequent arguments. This makes it
easy to customize app behavior for different machines in a cluster.
EOF
fi

mkdir build
cd build
rm -rf *

curl -L $1 -o dl.tar.gz
tar -zxvf dl.tar.gz
shift

dir=`find . -name run.sh | sed 's/run.sh$//'`
if [ -z "$dir" ]; then
    echo "No run.sh"
    exit 1
fi

cd $dir
/bin/sh run.sh $@
