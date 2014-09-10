Hacking SIP
==

First you need to install SIP. Nobody has a working package in the pip world,
so install from the tarball, which is available on sourceforge.

```bash
tar xfz sip-4.16.2.tar.gz 
(cd sip-4.16.2
python configure.py
make
sudo make install)
```

When you run `tryit.py`, it will build the SIP wrapper for the Word class defined by
`word.cpp` and `word.h`, and described by `word.sip`, and then try a usage example.