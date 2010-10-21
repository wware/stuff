==================
Fun with GTK and C
==================

At work I am doing some work with Damn Small Linux, making a bootable USB stick
to share some code with another company. I'm by no means a DSL guru, and I want
to whip up a graphical application, so I'm going to use GTK and C because I
believe GTK is present on DSL. I could use Perl, I guess, but they didn't include
Python. Maybe I'll try both Perl and C just to get a well-rounded experience.

I'll design the UI with Glade. I'm doing the development on an Ubuntu box. Of
course there's no C compiler on DSL. My experiences thusfar with MyDSL, the
"package manager" for DSL, have not been promising. You can download packages
but there's no way to actually install them.

It looks like dynamic linking in DSL is a non-starter, and you can't build a
statically linked GTK app because it depends on about a few thousand external
libraries.
