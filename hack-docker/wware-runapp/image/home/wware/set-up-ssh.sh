#!/bin/bash

rc4.py decrypt -i dot-ssh.tgz.encrypted | tar xfz -
chmod 500 .ssh
