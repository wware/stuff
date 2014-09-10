# Will's server image
#
# Stuff I want in my server

FROM      ubuntu:precise
MAINTAINER Will Ware <wware@alum.mit.edu>

# make sure the package repository is up to date
RUN echo "deb http://archive.ubuntu.com/ubuntu precise main universe" > /etc/apt/sources.list
RUN apt-get update

RUN apt-get install -y openssh-server python-setuptools curl python-redis redis-server \
	python-flask git

RUN easy_install pip virtualenv docopt

RUN adduser wware

ADD image /
