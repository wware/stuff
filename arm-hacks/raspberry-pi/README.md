Some fun Raspberry Pi hacks
=

Embedded system programming with JavaScript looks interesting. The idea here is
initially to use Node.JS on a Raspberry Pi, with
[ZeroMQ](http://www.slideshare.net/fedario/zero-mq-with-nodejs)
for interprocess communication
if needed. Embedded systems are well served by event-loop-style programming (think of
interrupt handlers) for which JavaScript is well suited, and shared-nothing message
queues are a great way to avoid most of the possible horrors with concurrent
programming.

Computation on the RPi
==

If ZeroMQ spans a network, one can build a
[Raspberry Pi cluster](http://likemagicappears.com/projects/raspberry-pi-cluster/),
maybe a little Map-Reduce machine. This would be especially cool if I got some skill
with GPU programming on the RPi
[1](http://petewarden.com/2014/08/07/how-to-optimize-raspberry-pi-code-using-its-gpu/)
[2](http://elinux.org/Raspberry_Pi_VideoCore_APIs)
[3](http://rpiplayground.wordpress.com/tag/raspberry-pi-gpu/). Or maybe a
[tuple-space-based](http://en.wikipedia.org/wiki/Linda_%28coordination_language%29)
system for high-speed computation.

Once upon a time, I designed a very naive sort of tuple space for Linux clusters, where
a single machine was the server, representing both a bottleneck and a single point of
failure. There are better implementations around now, checkout PyLinda and LIME.
Embarrassingly, my own is still listed on Wikipedia. For task queue management,
key-sharded [REDIS](http://redis.io/) probably beats a tuple space anyway.

SWIG with JavaScript
==

If I'm going to do embedded work in JS, I'll probably need to deal with C code at some
point, which means SWIG. So I did a little SWIG-nodejs hacking for fun.

```bash
sudo apt-get update
sudo apt-get install -y swig libzmq1 libzmq-dev

# Don't try to install nodejs/npm with apt-get.
wget http://nodejs.org/dist/v0.10.28/node-v0.10.28-linux-arm-pi.tar.gz
tar -xvzf node-v0.10.28-linux-arm-pi.tar.gz
cat >> /home/pi/.bash_profile <<EOF
NODE_JS_HOME=/home/pi/node-v0.10.28-linux-arm-pi
PATH=$PATH:$NODE_JS_HOME/bin
EOF
npm install -g node-gyp
```

ZeroMQ on a Raspbian machine
==

blah blah blah

Yocto for the RPi
==

A yocto build for the RPi would better acquaint me with all the yocto/poky/bitbake stuff,
and would enable me to customize an image for just the task I have in mind.

* http://www.cnx-software.com/2013/07/05/12mb-minimal-image-for-raspberry-pi-using-the-yocto-project/
* https://github.com/djwillis/meta-raspberrypi
* https://github.com/openembedded/openembedded/blob/master/recipes/zeromq/zeromq_2.1.6.bb
