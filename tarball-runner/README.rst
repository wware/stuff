Tarball Runner
==============

I want to make a customized Ubuntu DVD with the following things. It
should default to Taiwan Chinese for the language. It should have an
app called Tarball Runner which appears as an icon on the desktop when
it boots up. Tarball Runner can be open source, and nothing on this
DVD need be proprietary. But it provides a very simple way to share
proprietary programs with engineers in Taiwan, which run in Linux on
their Windows laptops.

Tarball Runner is a command-line Python script that takes the URL for
a tarball downloadable from the Internet, like this::

 http://willware.net/foobar.tar.gz

Then it asks for a password. If an empty password is given, it assumes
the tarball is unencrypted, otherwise it decrypts the tarball using
the Twofish algorithm.

Informative error messages should be provided if (a) the tarball is
not found at that URL, or (b) the decryption fails, which can be
detected by prepending four known bytes to the front of the cleartext
tarball, confirming them after decryption, and removing them after
confirmation. These informative error messages are to be available in
Chinese or English, depending on the F2 language choice the user made
at boot-up.

The tarball is unpacked into /tmp, and then the app tries to run
/tmp/run.sh.

The DVD should standardly include a lot of interpreted languages, a
full C development environment, and at least one GUI toolkit such as
GTK and PyGTK.

The source code repo for Tarball Runner should include the steps for
creating the full DVD and doing all the internationalization stuff,
and for how you'd use the thing.

Encryption is done with the Twofish algorithm devised by Bruce
Schneier in the mid-nineties, as implemented in Python by Eric Lee
Green and slightly modified by myself. The version used here is in my
Github repo at
http://github.com/wware/stuff/tree/master/twofishmodule-0.7/

Internationalization, meh
-------------------------

I've found that internationalization is a bit of a pain. The process
is extremely poorly documented online, and what documentation exists
is generally mutually conflicting. Since there are only about three
English words in Tarball Runner's UI, I'm giving up on this.

Testing
-------

The example directory has three files relevant to testing. run.sh is
the script that runs after the tarball is unpacked. example.tar.gz is
an unencrypted tarball containing only run.sh, which in this instance
just prints "HAPPY HAPPY JOY JOY". The file encrypted.tar.gz is a
version of the first tarball, encrypted with the password "Foobar".
To test this, either run the app by typing::

 file:///home/wware/stuff/tarball-runner/example/example.tar.gz

in the "Tarball" field with no password, or::

 file:///home/wware/stuff/tarball-runner/example/encrypted.tar.gz

in the "Tarball" field with the password "Foobar". In either case it
should deposit a copy of run.sh in the /tmp directory and print::

 HAPPY HAPPY JOY JOY

Preparing a tarball
-------------------

Create the tarball in an Ubuntu Linux environment similar to the one
in which the tarball will run. If it contains nothing secret or
proprietary, simply post it somewhere on the Internet. If it contains
proprietary information, encrypt it using the Twofish Python module
mentioned above, something like this::

 export TWOFISHDIR=/home/wware/stuff/twofishmodule-0.7
 python $TWOFISHDIR/cryptfile.py -iaaa.tar.gz -obbb.tar.gz
 # You will be prompted to provide a password.

where aaa.tar.gz is the plaintext tarball and bbb.tar.gz is the
encrypted tarball. Then post bbb.tar.gz on the Internet where your
intended recipient can download it. Email the URL for bbb.tar.gz and
the password to the recipient.
