# Will's server image
#
# Stuff I want in my server

FROM      ubuntu:bionic
MAINTAINER Will Ware <wware@alum.mit.edu>

# make sure the package repository is up to date
RUN apt-get update

# RUN apt-get install -y apt-utils python-setuptools python-dev build-essential curl python-redis redis-server python-flask
RUN apt-get install -y apt-utils python-redis redis-server python-flask

RUN apt-get install -y python-virtualenv python-docopt

ADD image /
