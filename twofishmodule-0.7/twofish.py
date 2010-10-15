# Python!
# Copyright 1999 Enhanced Software Technologies Inc.
#
# Released under an Open Source license. See file LICENSE.ocotillo 
# for licensing information.
#
# Written December 1999 by Eric Lee Green (eric@estinc.com)
#
# This is the front end for the _twofish module. It does error checking,
# throws exceptions, etc., as well as "padding" input strings that are
# too short to the nearest 128-bit boundary with random noise.
#
# TODO:
#
# fix the file operations. Fix the 'raise' statements to produce a usable
# exception value. Implement OFB/Counter mode. 

import _twofish  # the low level routines
import hashlib
import cryptrand # for the crytographically strong random # gen, sigh

NO_SALT="twofish.NO_SALT"
LOST_SYNC="twofish.LOST_SYNC"
NO_KEY="twofish.NO_KEY"
BAD_KEY="twofish.BAD_KEY"
BAD_BLOCK_SIZE="twofish.BAD_BLOCK_SIZE"


class twofish:
    def __init__(self,key=None):
	self.random=cryptrand.cryptrand()
	self.cipher=_twofish.new()
	self.keyed=None
	self.cfb_salted=None  # we're not salted! 
	self.cfb_salt=None # next data to be crypted
	self.cfb_crypted=None # the encrypted salt. 
	self.cfb_idx=-1

	if key:
	    self.setkey(key)
	    pass
	return

    # set the key. Raise twofish.bad_key if key is invalid. 
    def setkey(self,key):
	i=len(key)
	# see if it's a valid key length. If not, throw an exception.
	if i != 16 and i != 24 and i != 32:
	    raise BAD_KEY,"Bad Key Length: %s" % i
	self.cipher.set_key(key)
	self.keyed=1  # We are now keyed. 
	return
    
    # This is where we hash a string and use it as the key to the cipher.
    def hashkey(self,string):
	m = hashlib.md5()
	m.update(string)
	key = m.digest()
	self.setkey(key) # set the key to the md5 hash of the input string!
	return
    
    # this is where we encrypt stuff. Accepts arbitrary number of chars
    # as long as it is a multiple of 16 in length. If it is not a multiple 
    # of 16 in length, we pad it to 16 chars. Needs 'cryptrand', sigh...
	
    def encrypt(self,instr):
	if not self.keyed:
	    raise NO_KEY,"No Key Set for Cipher"
	retvalue=""
	while len(instr) >= 16:
	    s=instr[0:16]     # set it to first 16 chars 
	    instr=instr[16:]  # chop off 16 chars
	    retvalue= retvalue + self.cipher.encrypt(s)
	    pass
	# okay, now for the last 16 chars:
	if not instr:   # if it's null  then ...
	    return retvalue
	
	numneeded=16-len(instr) # number of random chars we need.
	s=instr+self.random.rand(numneeded)
	retvalue=retvalue+self.cipher.encrypt(s)
	return retvalue  # and return! 
    
    # This is where we DECRYPT stuff. It *MUST* be a multiple of 16 in
    # length, else we WILL NOT decrypt it, period.
    
    def decrypt(self,instr):
	if not self.keyed:
	    raise NO_KEY,"No Key Set for Cipher"
	i=len(instr) % 16
	if i:
	    raise BAD_BLOCK_SIZE,"Twofish uses 16-byte block size"
	retvalue=""
	while len(instr) >= 16:
	    s=instr[0:16]
	    instr=instr[16:]
	    retvalue = retvalue + self.cipher.decrypt(s)
	    pass
	# now we should be able to return it!
	return retvalue
    
    # This is cipher block chaining mode with a random initial salt. This way
    # the ciphertext never looks exactly like the 
    # It returns: 16 bytes of random data
    #             Encrypted data padded to next 16 byte boundary with nulls.
    #
    
    def encrypt_cbc(self,instr):
	if not self.keyed:
	    raise NO_KEY,"No Key Set for Cipher"
	if not instr:
	    return ""  # sorry, nothing to encrypt :-(.
	last_block=self.random.rand(16)  # random numbers.
	retvalue=last_block

	while len(instr) >= 16:
	    s=instr[0:16]     # set it to first 16 chars 
	    instr=instr[16:]  # chop off 16 chars
	    # now to xor s with the previous ciphertext block:
	    new_plaintext=self.cipher.xor_block(s,last_block)
	    # now set the last_block to the encipherment of that value:
	    last_block=self.cipher.encrypt(new_plaintext)
	    # Now add that encipherment to the return value.
	    retvalue= retvalue + last_block 
	    pass
	# okay, now for the last 16 chars:
	if not instr:   # if it's null  then ...
	    return retvalue
	
	numneeded=16-len(instr) # number of random chars we need.
	s=instr+self.random.rand(numneeded)
	new_plaintext=self.cipher.xor_block(s,last_block)
	retvalue=retvalue+self.cipher.encrypt(new_plaintext)
	return retvalue  # and return! 

    # This is cipher feedback mode with a random initial salt. The
    # initial 16 bytes of the input are totally random and are 
    # discarded. The remainder must be 16-byte aligned. 
    def decrypt_cbc(self,instr):
	if not self.keyed:
	    raise NO_KEY,"No Key Set for Cipher"
	if not instr:
	    return ""  # sorry, nothing to encrypt!
	i=len(instr) % 16
	if i:
	    raise BAD_BLOCK_SIZE,"Twofish uses 16-byte block size"
	if len(instr) < 32:
	    return ""  # sorry, all we had was the random initial block!
	last_block=instr[0:16]  # okay, get the xor data 
	instr=instr[16:]        # and strip it off the front...
	retvalue=""
	while len(instr) >= 16:
	    s=instr[0:16]
	    instr=instr[16:]
	    new_xortext=self.cipher.decrypt(s)
	    new_plaintext=self.cipher.xor_block(new_xortext,last_block)
	    last_block=s # new ciphertext to decrypt with :-). 
	    retvalue = retvalue + new_plaintext
	    pass
	# now we should be able to return it!
	return retvalue


    # we salt ourself for CFB modes, then return that 16-byte initial 
    # random salt value. 
    def salt(self,value=None):
	self.cfb_salted=1
	if value:
	    self.cfb_salt=value
	else:
	    self.cfb_salt=self.random.rand(16)  # random numbers
	    pass
	self.cfb_crypted=None               # we're not crypted yet... 
	self.cipher.cfb_salt(self.cfb_salt) # for the low-level CFB8 mode. 
	self.cfb_idx=-1  # un-salt us!
	return self.cfb_salt
    
	
    # This is cipher feedback mode 128bit with a random initial salt. This way
    # the ciphertext never looks exactly like the code book value.
    # CFB mode emulates a stream cipher, i.e., the input is the same length as
    # the output (with a random salt). You must first call the 'salt' routine
    # to set up a random salt.... this is so that we can be repeated called
    # by our callers. 

    # It returns:  Encrypted data encrypted in cfb128 mode

    
    def encrypt_cfb128(self,instr):
	if not self.keyed:
	    raise NO_KEY,"No Key Set for Cipher"
	if not instr:
	    return ""  # sorry, nothing to encrypt :-(.
	if not self.cfb_salted:
	    raise NO_SALT,"No CFB salt set for cipher."

	retval=self.cipher.cfb_encrypt128(instr)
	return retval

    # I had to put cfb128 into "C" for performance reasons. 
    # if you want to know what the above USED to look like: The
    # performance difference: encrypting /etc/termcap went from
    # 40 seconds down to 0.4 seconds!
    def dummy1(self):
	retval=""

	for s in instr:
	    if self.cfb_idx < 0 or self.cfb_idx > 15:
		self.cfb_crypted=self.cipher.encrypt(self.cfb_salt)
		self.cfb_salt=""
		self.cfb_idx=0
		pass
	    ch=ord(s) # get ascii value
	    maskch=ord(self.cfb_crypted[self.cfb_idx])
	    cryptch= ch ^ maskch
	    resultch=chr(cryptch)
	    self.cfb_salt = self.cfb_salt + resultch
	    self.cfb_idx = self.cfb_idx + 1
	    retval = retval + resultch
	    continue
	
	return retval

    # now to decrypt

    # The caller must set up our IV by calling self.salt()... otherwise, our
    # results are undefined. 
    def decrypt_cfb128(self,instr):
	if not self.keyed:
	    raise NO_KEY,"No Key Set for Cipher"
	if not instr:
	    return ""  # sorry, nothing to encrypt :-(.
	if not self.cfb_salted:
	    raise NO_SALT,"No CFB salt set for cipher."

	retval=self.cipher.cfb_decrypt128(instr)
	return retval

    # Note that I had to move cfb128 into "C" for performance reasons.
    # went from 40 seconds to 0.4 seconds to decrypt /etc/termcap!
    # just so you can see what the code *USED* to look like, here it is:
    def dummy2(self):

	retval=""

	for s in instr:
	    if self.cfb_idx < 0 or self.cfb_idx > 15:
		self.cfb_crypted=self.cipher.encrypt(self.cfb_salt)
		self.cfb_salt=""
		self.cfb_idx=0
		pass
	    cryptch=ord(s) # get ascii value
	    maskch=ord(self.cfb_crypted[self.cfb_idx])
	    plainch= cryptch ^ maskch
	    resultch=chr(plainch)
	    self.cfb_salt = self.cfb_salt + s
	    self.cfb_idx = self.cfb_idx + 1
	    retval = retval + resultch
	    continue
	
	return retval

    # The following uses CFC128 mode to encrypt and decrypt files...

    
    # Encrypt a file, sending output to another file. Actually, will
    # accept any objects that have "read(size)" and "write(data)" methods. 
    def encrypt_cfb_file(self,infile,outfile):
	if not self.keyed:
	    raise NO_KEY,"No Key Set for Cipher"

	salt=self.salt()
	outfile.write(salt)
	s=infile.read(16384)
	while s:
	    outfile.write(self.cipher.cfb_encrypt128(s))
	    s=infile.read(16384)
	    pass
	return # no return value :-(.
    
    # Decrypt a file, sending output to another file. Actually, will
    # accept any objects that have read(size) and write(data) methods.
    
    def decrypt_cfb_file(self,infile,outfile):

	if not self.keyed:
	    raise NO_KEY,"No Key Set for Cipher"
	salt=infile.read(16)
	if not salt:
	    raise NO_SALT,"could not read 16-byte initialization value from file!"
	self.salt(salt)
	# now for the rest.....
	s=infile.read(16384)
	while s:
	    outfile.write(self.cipher.cfb_decrypt128(s))
	    s=infile.read(16384)
	    pass
	return  # no return value :-(
    
    # CFB-8 bit. It's implemented in "C". 
    # Now for a cipher-feedback mode: This mode works like a stream
    # cipher. It does NOT pad the last block with gibberish. Though it
    # *DOES* prepend a 16-byte salt to the start. See Schneir's book.
    # *WARNING* The 'salt' only works if you change the keys regularly!
    # Otherwise the fact that this is a simple XOR stream cipher (basically
    # using the crypto-engine only as a source of pseudo-random numbers)
    # will cause us to be basically plain English!
    # See Schneir's book. 
    
    def encrypt_cfb(self,instr,shiftreg=None):
	if not self.keyed:
	    raise NO_KEY,"No Key Set for Cipher"
	# if we were not fed a shiftreg, create a randome one:
	if not shiftreg:
	    shiftreg=self.random.rand(16)  # 16 bytes worth!
	    pass
	if len(shiftreg) != 16:
	    raise "twofish.bad_salt","Bad salt for CFB mode: %s" % shiftreg
	self.cipher.cfb_salt(shiftreg) # always succeeds.
	if not instr:
	    return None  # no result!
	s=self.cipher.cfb_encrypt(instr)
	return shiftreg+s
    
    # This decrypts a CFB-encrypted string. The salt is the first sixteen
    # characters. If the length of the input string is less than 17 chars,
    # we return None!
    def decrypt_cfb(self,instr):
	if not self.keyed:
	    raise NO_KEY,"No Key Set for Cipher"
	if len(instr) < 17:
	    return None
	# Now to salt the shift register:
	self.cipher.cfb_salt(instr[0:16])
	instr=instr[16:]
	# and return the decrypted string. 
	return self.cipher.cfb_decrypt(instr)  # and done! 
    
    
# Test program, to test some test vectors:
def test():
    import bin2hex  # bin2hex conversions.

    a_key=[ "00000000000000000000000000000000",
          "00000000000000000000000000000000",
          "9F589F5CF6122C32B6BFEC2F2AE8C35A",
          "D491DB16E7B1C39E86CB086B789F5419",
          "019F9809DE1711858FAAC3A3BA20FBC3",
          "6363977DE839486297E661C6C9D668EB",
          "816D5BD0FAE35342BF2A7412C246F752"
        ]
    a_pt=[ "00000000000000000000000000000000",
         "9F589F5CF6122C32B6BFEC2F2AE8C35A",
         "D491DB16E7B1C39E86CB086B789F5419",
         "019F9809DE1711858FAAC3A3BA20FBC3",
         "6363977DE839486297E661C6C9D668EB",
         "816D5BD0FAE35342BF2A7412C246F752",
         "5449ECA008FF5921155F598AF4CED4D0"

        ]
    a_ct=[ "9F589F5CF6122C32B6BFEC2F2AE8C35A",
         "D491DB16E7B1C39E86CB086B789F5419",
         "019F9809DE1711858FAAC3A3BA20FBC3",
         "6363977DE839486297E661C6C9D668EB",
         "816D5BD0FAE35342BF2A7412C246F752",
         "5449ECA008FF5921155F598AF4CED4D0",
         "6600522E97AEB3094ED5F92AFCBCDD10"
        ]

    key=[]
    pt=[]
    ct=[]
    

    for s in a_key:
        # t=bin2hex.hex2bin(s)
        t=bin2hex.reverse(bin2hex.hex2bin(s))
        key.append(t)
        continue
    for s in a_pt:
        t=bin2hex.hex2bin(s)
        # t=bin2hex.reverse(bin2hex.hex2bin(s))
        pt.append(t)
        continue
    for s in a_ct:
        # t=bin2hex.reverse(bin2hex.hex2bin(s))
        t=bin2hex.hex2bin(s)
        ct.append(t)
        continue

    c=twofish() # set our cipher!
    i=0
    for k in key:
        print "[%s:%s:%s]" % (i,len(k),bin2hex.bin2hex(k)),
        c.setkey(k)
        r=c.encrypt(pt[i])
        if r != ct[i]:
            print "Vector",i,":"
            print "  Should be: ",a_ct[i]
            print "         Is: ",bin2hex.bin2hex(r)
            pass
        i=i+1
        continue
    print "------Test Finished-----------"
    pass

    
