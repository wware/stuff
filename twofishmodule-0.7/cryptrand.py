# Python!
#
# Copyright 1999 Enhanced Software Technologies Inc.
# All rights reserved.
#
# Python method that is the front end for whatever random number
# generator we are using. Currently, it only uses /dev/urandom as
# its random number generator and thus will operate properly only under
# FreeBSD and Linux, but eventually Ocotillo will be added to the Python
# distribution as "ocrand" and fix this.
#
# RCS CHANGE LOG
# Revision 1.1  1999/11/30 17:24:58  eric
# Checkin.
#
# Revision 1.2  1999/10/28 16:45:45  eric
# Added copyright notice and RCS change log
#
#

class cryptrand:
    def __init__(self,seed=None):
        # find our source of randomness: we currently ignore the seed!

        self.seed=seed
        fh=open("/dev/urandom","r") # FIX THIS FOR OTHER OS's!
        if not fh:
            raise "NO_RANDOM_SOURCE"
        self.fh=fh
        return

    # this actually returns random data.
    def rand(self,bytes):
        s=self.fh.read(bytes)
        return s

    # in the ocrandom implementation, this "stirs" new entropy into
    # the mix.
    def stir(self,bytes):
        pass
