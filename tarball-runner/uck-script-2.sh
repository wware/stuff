#!/bin/sh

# This is an installation script to be run while creating an ISO file
# using Ubuntu Customization Kit. When you get to where UCK asks you to
# run a console app, run this script in the chroot jail after runnning
# uck-script-1.sh on the host machine.

sudo apt-get install -y python-dev

(cd /twofishmodule-0.7
./configure
make
make install)

rm -rf /tmp/uck-script-2.sh
