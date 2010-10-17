#!/bin/sh

# This is an installation script to be run while creating an ISO file
# using Ubuntu Customization Kit. When you get to where UCK asks you to
# run a console app, run this script on the host machine, then run
# /tmp/uck-script-2.sh in the chroot jail.

sudo cp ${HOME}/stuff/tarball-runner/uck-script-2.sh \
    ${HOME}/tmp/remaster-root/tmp

sudo cp -R ../twofishmodule-0.7 ${HOME}/tmp/remaster-root
sudo cp tarballrunner.py ${HOME}/tmp/remaster-root/bin
