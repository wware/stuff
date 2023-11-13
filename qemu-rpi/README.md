# QEMU Raspberry Pi stuff

I've long wanted to use QEMU to run Raspberry Pi code on a laptop where I could comfortably get it into the shape
I want it to be in. I tackled this on a Linux laptop this past weekend, with a lot of success. I started out using
[this tutorial](https://azeria-labs.com/emulate-raspberry-pi-with-qemu/), and my only complaint with it is that
the tutorial is some years old and uses old versions of things. So I'll probably try to update those files at some
point.

A more recent Raspbian operating system image would be
[this one from 2020-02-14](https://downloads.raspberrypi.org/raspbian/images/raspbian-2020-02-14/2020-02-13-raspbian-buster.zip).
A more recent kernel file might be
[this one](https://github.com/dhruvvyas90/qemu-rpi-kernel/blob/master/kernel-qemu-5.4.51-buster).

I scribbled a few notes....

> This is working and I can use this to create SD cards to run on a real RPi, and to create docker images that work on an RPi.
>
> Look at Haskell and Rust and OCaml on the RPi. Those are good ways to create docker images.
>
> Look at using more modern files than the ones in that blog post, and installing the docker runner.

Unfortunately my work laptop tends to be locked down for work security reasons so I can only do this stuff on the
old personal Linux laptop.

But the nice thing about all this is that I can cook up the RPi side of hobby projects quite easily, and then
I don't have to put up with utter crap for software.

# Relevant Raspberry Pi resources

## Docker

* https://docs.docker.com/engine/install/raspberry-pi-os/
* https://www.simplilearn.com/tutorials/docker-tutorial/raspberry-pi-docker

## Haskell

* https://wiki.haskell.org/Raspberry_Pi
* https://github.com/blitzcode/hue-dashboard/blob/master/README.md#raspberry-pi
* https://svejcar.dev/posts/2019/09/23/haskell-on-raspberry-pi-4/
* https://www.reddit.com/r/haskell/comments/d86sqc/my_experience_with_haskell_on_raspberry_pi_4b_4gb/
* https://medium.com/@zw3rk/a-haskell-cross-compiler-for-raspberry-pi-ddd9d41ced94

## Rust

* https://harmonicss.co.uk/bare-metal/rust-on-a-raspberry-pi-part-1/
* https://forums.raspberrypi.com/viewtopic.php?p=2119201
* https://www.reddit.com/r/rust/comments/vparsp/has_anyone_programmed_a_raspberry_pi_with_rust/
* https://github.com/rust-embedded/rust-raspberrypi-OS-tutorials
* https://www.freecodecamp.org/news/embedded-rust-programming-on-raspberry-pi-zero-w/

## OCaml

* https://blog.mro.name/2020/04/bootstrap-raspi4-ocaml/
* https://dannywillems.github.io/2016/06/22/ocaml-raspberry-pi.html
* https://github.com/dbuenzli/rpi-boot-ocaml/blob/master/README.md
* https://blog.janestreet.com/using-ocaml-to-drive-a-raspberry-pi-robot-car/
* https://www.raspberryconnect.com/raspbian-packages/51-raspbian-ocaml


