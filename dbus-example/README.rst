Fooling with DBus
=================

I know dbus is important but I don't know much about it. I know that
it's a popular inter-process communication system used in Gnome, with
some connections to the kernel. It seems to me it would be good to
learn about it.

So my little project idea is to set up some service, maybe a physics
engine (probably ODE) as a dbus server, and clients can come to it and
get physical simulations done by it. Curious question: is ODE multi-
threaded in a way that two clients could simultaneously have it work
on two unrelated physics simualtions?

I found these packages already installed::

 dbus
 dbus-x11
 libdbus-1-3
 libdbus-1-dev
 python-dbus

Here are the Ubuntu packages I installed in order to be able to do
this::

 libdbus-glib-1-2
 libdbus-glib-1-dev
 libdbus-glib-1-doc

These doc packages are potentially useful::

 libdbus-1-doc
 libdbus-doc
 dbus-1-doc
 python-dbus-doc

I want to do some more with DBus, and package it as a debian package
(both a bin package and a src package), and merge this with the
make-debian-package directory.
