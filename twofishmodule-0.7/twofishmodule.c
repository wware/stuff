/* Copyright 1999 Enhanced Software Technologies Inc.
 * All Rights Reserved
 *
 * This module is released under a BSD-style Open Source license.
 * See file LICENSE for licensing information. 
 *
 * Written December 1999 Eric Lee Green
 *
 */
/* Twofish Module:
 * This module provides an interface to the TwoFish encryption algorithm,
 * as written by Bruce Schneir et. al. at Counterpane Systems 
 * (http://www.counterpane.com). Dr. Brian Gladman's sample AES implementation
 * is used to provide the "twofish.c" and "std_defs.h" code needed here. 
 *
 * Note: This implementation is generously cribbed from md5module.c, which
 * had some of the same criteria. I have heavily hacked Dr. Gladman's code
 * to make it fit into that same framework.
 */


#include "Python.h"
#include "aes.h"

typedef unsigned char u1byte;

typedef struct {
  PyObject_HEAD
  keyInstance encrypt_key;  /* state info for the Twofish cipher... */
  keyInstance decrypt_key;  
  int key_gen;  /* a flag for whether we're keyed or not... */
  cipherInstance cipher;
  u1byte cfb_blk[16];
  u1byte cfb_crypt[16]; /* encrypted cfb_blk for CFB128 mode */
  int cfb128_idx;       /* where we are in the CFB128 cfb_blk). */ 
} TwoFishObject;

staticforward PyTypeObject TwoFishType;

static PyObject *TwoFishError;

#define is_TwoFishObject(v)  ((v)->ob_type == &TwoFishType);


/****************
 * The NEW method....
 ****************/
static TwoFishObject *newTwoFishObject(void) {
  TwoFishObject *fishp;
  
  fishp=PyObject_NEW(TwoFishObject,&TwoFishType);
  if (fishp == NULL)
    return NULL;
  cipherInit(&fishp->cipher,MODE_ECB,NULL);
  fishp->cfb128_idx=-1;  /* start @ start of cfb_blk... */  \
  fishp->key_gen=0; /* have not done a key yet... */
  return fishp;
}

static char new_doc [] = 
"new() -> _twofishobject:\n"
"\n"
"Return a new TwoFish object\n";


/**************
* The DEL method:
***************/
static void
twofish_dealloc(TwoFishObject *fishp) {
  PyMem_DEL(fishp);  /* add it back to the free object list. */
}

/***************
 * The set_key method:
 ***************/

static PyObject *twofish_set_key(TwoFishObject *self, PyObject *args) {
  unsigned char *key;
  int key_len;   /* sorry, this is what '#' parses :-(. */
  int i;
  
  if (!PyArg_Parse(args,"s#",&key,&key_len)) {
    return NULL;
  }

  if (key_len != 16 && key_len != 24 && key_len != 32) {
    /* Okay, we must raise an exception and exit :-(. */

    /****FIXME****/
    return NULL; 
  }

  /* okay, we do have it.... */

  /* sigh, double key schedule set up time. Sigh sigh sigh. */
  makeKey(&self->encrypt_key,DIR_ENCRYPT,key_len*8,NULL);
  makeKey(&self->decrypt_key,DIR_DECRYPT,key_len*8,NULL);
  memcpy(self->encrypt_key.key32, key, key_len);
  memcpy(self->decrypt_key.key32, key, key_len);
  reKey(&self->encrypt_key);
  reKey(&self->decrypt_key);

  self->key_gen=1;    /* voila! */
  
  Py_INCREF(Py_None);
  return Py_None;  /* always succeeds. */
}

static char set_key_doc [] =
"set_key(arg): \n"
"Set the key for this Twofish object. The key must be 128, 192, or 256 \n"
"bits in length (16, 24, or 32 characters). Raises twofish.error with a\n"
"value of 'Invalid Key' if the key is any other length.\n";

static PyObject *twofish_encrypt(TwoFishObject *self,PyObject *args) {
  unsigned char *data;
  int data_len;

  u1byte out_blk[16];

  if (!PyArg_Parse(args,"s#",&data,&data_len)) {
    return NULL;
  }

  if (data_len != 16) {
    /* **FIXME**  Okay, raise exception here. */
    return NULL;
  }

  if (!self->key_gen) {
    /* sorry, no key has been initialized! */
    /* ***FIXME*** RAISE EXCEPTION HERE */
    return NULL; 
  }
  /* okay, now to encrypt it: */
  blockEncrypt(&self->cipher,&self->encrypt_key,data,128,out_blk);

  /* encrypt(self->ctx,(u1byte *) data,out_blk); */
  
  /* Now to create a Python string out of the result: */
  return PyString_FromStringAndSize((char *)out_blk,16);
}

static char encrypt_doc[] = "encrypt(data)->string\n"
"Given a 128-bit data value, returns that value encrypted with the current\n"
"key. If the data is other than 128 bits, raises an exception. If no key \n"
"has been set up, raises an exception.\n";

/** CFB mode helper routines.... */

static PyObject *cfb_salt(TwoFishObject *self,PyObject *args) {
  unsigned char *src;
  unsigned char *dest;
  int src_len;
  int i;

  self->cfb128_idx=-1;
  dest=self->cfb_blk;
  if (!PyArg_Parse(args,"s#",&src,&src_len)) {
    return NULL;  /* higher level should check this... */
  }
  
  if (src_len != 16) {
    return NULL; /* source length *MUST* be 16! */
  }

  for (i=0;i<16;i++) {
    *dest++ = *src++;
  }
  Py_INCREF(Py_None);
  return Py_None;  /* Always succeeds. */
}

static char cfb_salt_doc[] =
"cfb_salt(salt):\n"
"\n"
"When given a 16-byte salt value, stores it in the _twofish object's\n"
"internal salt buffer for use in further CFB encryptions.\n";

/*** This is an internal routine which has no external interface. ***/
static int cfb_decrypt_char(TwoFishObject *self,int ch) {
  int result;
  u1byte crypt_blk[16];
  int i;
  
  /* Okay, first encrypt the CFB shift register: */
  blockEncrypt(&self->cipher,&self->encrypt_key,self->cfb_blk,128,crypt_blk);

  /* encrypt(self->ctx,self->cfb_blk,crypt_blk); */

  /* Now XOR the lowbyte of the encrypted CFB register with the cryptotext.*/
  result= ch ^ (unsigned int)crypt_blk[0];
  /* Now shift the register from the RIGHT: */
  for (i=0;i<15;i++) {
    self->cfb_blk[i]=self->cfb_blk[i+1];
  }
  self->cfb_blk[15]=(u1byte)ch; /* and add the cryptotext to the shift reg. */
  return result;
}

/*** This is an internal routine which has no external interface. ***/

static int cfb_encrypt_char(TwoFishObject *self,int ch) {
  int result;
  u1byte crypt_blk[16];
  int i;
  
  /* okay, encrypt the CFB shift register: */
  blockEncrypt(&self->cipher,&self->encrypt_key,self->cfb_blk,128,crypt_blk);

  /*  encrypt(self->ctx,self->cfb_blk,crypt_blk);  */

  /* Now XOR the low byte of the encrypted CFB register with the plaintext. */
  result=ch ^ (unsigned int) crypt_blk[0];
  
  /* And shift it into the crypt_blk from the RIGHT: */
  for (i=0;i<15;i++) {
    self->cfb_blk[i]=self->cfb_blk[i+1];
  }
  self->cfb_blk[15]=(u1byte)result;
  return result;

}

/* Now for the actual cfb_encrypt routine: */
static PyObject *cfb_encrypt(TwoFishObject *self,PyObject *args) {
  unsigned char *src;
  unsigned char *destdata;
  int srclen;
  int i;

  PyObject *retvalue;
  
  if (!PyArg_Parse(args,"s#",&src,&srclen)) {
    return NULL; /* higher level should check this. */
  }
  
  /* Now to alloc our result buffer: will be same length as original data. */
  destdata=(unsigned char *) malloc(srclen);

  /* Now to encrypt each individual character in cfb mode: */
  for (i=0; i<srclen; i++) {
    destdata[i]=(unsigned char)cfb_encrypt_char(self,src[i]);
  }

  /* Now create a return value: */
  retvalue=PyString_FromStringAndSize((char *)destdata,srclen);
  /* free our malloced memory... */
  free(destdata);
  return retvalue; /* and return the Python string object! */
}

static char cfb_encrypt_doc[] =
"cfb_encrypt(data)->string\n"
"Given a stream of bytes, returns a stream of bytes encrypted in CFB \n"
"(Cipher FeedBack) mode. See Schneir, page 200. Note: Must first call\n"
"cfb_salt(data) to set up the initial unique salt value!\n";

static char cfb_encrypt128_doc[] =
"cfb_encrypt(data)->string\n"
"Given a stream of bytes, returns a stream of bytes encrypted in CFB128 \n"
"(Cipher FeedBack) mode. See Schneir, page 200. Note: Must first call\n"
"cfb_salt(data) to set up the initial unique salt value!\n";



/* Now for the actual cfb_encrypt routine: */
static PyObject *cfb_encrypt128(TwoFishObject *self,PyObject *args) {
  unsigned char *src;
  unsigned char *destdata;
  int srclen;
  int i,ch;
 
  PyObject *retvalue;
  
  if (!PyArg_Parse(args,"s#",&src,&srclen)) {
    return NULL; /* higher level should check this. */
  } 
  
  /* Now to alloc our result buffer: will be same length as original data. */
  destdata=(unsigned char *) malloc(srclen);

  /* Now to encrypt each individual character in cfb mode: */
  for (i=0; i<srclen; i++) {
    if ((self->cfb128_idx < 0) || (self->cfb128_idx > 15)) {
       blockEncrypt(&self->cipher,&self->encrypt_key,self->cfb_blk,128,self->cfb_crypt);

      /* encrypt(self->ctx,self->cfb_blk,self->cfb_crypt); */
      self->cfb128_idx=0;
    }
    /* XOR the data with a byte from our encrypted buffer. */ 
    ch=src[i] ^ self->cfb_crypt[self->cfb128_idx];
    /* do output feedback: put crypted byte into next block to be crypted */
    self->cfb_blk[self->cfb128_idx++]=ch; 
    destdata[i]=(unsigned char) ch;
  }

  /* Now create a return value: */
  retvalue=PyString_FromStringAndSize((char *)destdata,srclen);
  /* free our malloced memory... */
  free(destdata);
  return retvalue; /* and return the Python string object! */
}



/* Now for the actual cfb_decrypt routine: */
static PyObject *cfb_decrypt(TwoFishObject *self,PyObject *args) {
  unsigned char *src;
  unsigned char *destdata;
  int srclen;
  int i;

  PyObject *retvalue;
  
  if (!PyArg_Parse(args,"s#",&src,&srclen)) {
    return NULL; /* higher level should check this. */
  }
  
  /* Now to alloc our result buffer: will be same length as original data. */
  destdata=(unsigned char *) malloc(srclen);

  /* Now to encrypt each individual character in cfb mode: */
  for (i=0; i<srclen; i++) {
    destdata[i]=(unsigned char)cfb_decrypt_char(self,src[i]);
  }

  /* Now create a return value: */
  retvalue=PyString_FromStringAndSize((char *)destdata,srclen);
  /* free our malloced memory... */
   free(destdata);
   return retvalue; /* and return the Python string object! */
}

static char cfb_decrypt_doc[] =
"cfb_decrypt(data)->string\n"
"Given a stream of bytes, returns a stream of bytes decrypted in CFB \n"
"(Cipher FeedBack) mode. See Schneir, page 200. Note: Must first call\n"
"cfb_salt(data) to set up the initial unique salt value!\n";


static char cfb_decrypt128_doc[] =
"cfb_decrypt128(data)->string\n"
"Given a stream of bytes, returns a stream of bytes decrypted in CFB128 \n"
"(Cipher FeedBack) mode. See Schneir, page 200. Note: Must first call\n"
"cfb_salt(data) to set up the initial unique salt value!\n";

 
/* Now for the actual cfb_decrypt routine: */
static PyObject *cfb_decrypt128(TwoFishObject *self,PyObject *args) {
  unsigned char *src;
  unsigned char *destdata;
  int srclen;
  int i;
  unsigned char ch;
  
  PyObject *retvalue;
  
  if (!PyArg_Parse(args,"s#",&src,&srclen)) {
    return NULL; /* higher level should check this. */
  }
 
  /* Now to alloc our result buffer: will be same length as original data. */
  destdata=(unsigned char *) malloc(srclen);
 
  /* Now to encrypt each individual character in cfb mode: */
  for (i=0; i<srclen; i++) {
    if (self->cfb128_idx < 0 || self->cfb128_idx > 15) {

      blockEncrypt(&self->cipher,&self->encrypt_key,self->cfb_blk,128,self->cfb_crypt);
           

      /* encrypt(self->ctx,self->cfb_blk,self->cfb_crypt); */
      self->cfb128_idx=0;
    }
    ch=src[i];
    destdata[i]=ch ^ self->cfb_crypt[self->cfb128_idx]; 
    /* do output feedback: put crypted byte into next block to be crypted */
    self->cfb_blk[self->cfb128_idx++]=ch; 
  }
  /* Now create a return value: */
  retvalue=PyString_FromStringAndSize((char *)destdata,srclen);
  /* free our malloced memory... */
  free(destdata);
  return retvalue; /* and return the Python string object! */
}

/* Now for an XOR mode: */

static PyObject *xor_block(TwoFishObject *self,PyObject *args) {
  unsigned char *src;
  unsigned char *xordata;
  int src_len;
  int xordata_len;
  int i;

  u1byte return_blk[16];

  if (!PyArg_ParseTuple(args,"s#s#",&src,&src_len,&xordata,&xordata_len)) {
    return NULL; /* higher level should check this... */
  }

  if (src_len != 16 || xordata_len != 16) {
    return NULL;
  }
  
  /* Now xor each character in the return block. */
  for (i=0;i<16;i++) {
    return_blk[i]=*src++ ^ *xordata++;
  }

  /* Now to create a Python string out of the result: */
  return PyString_FromStringAndSize((char *)return_blk,16);
}

static char xor_block_doc[] =
"xor_block(data,mask)->string\n"
"Given a 128 bit data value, and a 128 bit mask, xor (exclusive-or)'s the\n"
"data value with the mask. This routine is intended for use by the CFB mode\n"
"and like all low level _twofish routines is not intended to be called\n"
"directly.\n";


/* Now for the decrypt: */

static PyObject *twofish_decrypt(TwoFishObject *self,PyObject *args) {
  unsigned char *data;
  int data_len;

  u1byte out_blk[16];

  if (!PyArg_Parse(args,"s#",&data,&data_len)) {
    return NULL;
  }

  if (data_len != 16) {
    /* **FIXME**  Okay, raise exception here. */
    return NULL;
  }

  if (!self->key_gen) {
    /* sorry, no key has been initialized! */
    /* ***FIXME*** RAISE EXCEPTION HERE */
    return NULL; 
  }
   /* okay, now to decrypt it: */

     blockDecrypt(&self->cipher,&self->decrypt_key,data,128,out_blk);
  /*   decrypt(self->ctx,(u1byte *) data,out_blk); */

  
  /* Now to create a Python string out of the result: */
  return PyString_FromStringAndSize((char *)out_blk,16);
}

static char decrypt_doc[] = "decrypt(data)->string\n"
"Given a 128-bit data value, returns that value decrypted with the current\n"
"key. If the data is other than 128 bits, raises an exception. If no key \n"
"has been set up, raises an exception.\n";


static PyMethodDef twofish_methods[] = {
  {"set_key", (PyCFunction)twofish_set_key,0,set_key_doc},
  {"encrypt", (PyCFunction)twofish_encrypt,0,encrypt_doc},
  {"decrypt", (PyCFunction)twofish_decrypt,0,decrypt_doc},
  {"xor_block", (PyCFunction)xor_block,0,xor_block_doc},
  {"cfb_salt", (PyCFunction)cfb_salt,0,cfb_salt_doc},
  {"cfb_encrypt",(PyCFunction)cfb_encrypt,0,cfb_encrypt_doc},
  {"cfb_decrypt",(PyCFunction)cfb_decrypt,0,cfb_decrypt_doc},
  {"cfb_encrypt128",(PyCFunction)cfb_encrypt128,0,cfb_encrypt128_doc},
  {"cfb_decrypt128",(PyCFunction)cfb_decrypt128,0,cfb_decrypt128_doc},

  {NULL,       NULL,   0, NULL}  /* sentinel */
};

/* GetAttr function */
static PyObject *twofish_getattr(TwoFishObject *self, char *name) {
  return Py_FindMethod(twofish_methods,(PyObject *)self, name);
}

static char module_doc [] = 
"This module implements the interface to Dr. Brian Gladman's implementation\n"
"of Counterpane's 'twofish' encryption algorithm. This is the low-level\n"
"interface that operates solely upon 128-bit data values. For stream-oriented\n"
"variants, see the 'twofish.py' module.\n"
"\n"
"Functions:\n"
"new() -- return a new twofish object.\n"
"\n"
"Special Objects:"
"\n"
"TwoFishType -- type object for TwoFish objects. \n";

static char TwoFishType_doc [] =
"A TwoFishType represents an object that applies the TwoFish encryption and\n"
"decryption algorithms to data. This low-level type operates solely upon\n"
"128-bit data values and accepts 128, 192, or 256-bit key values. \n"
"\n"
"Methods:\n"
"\n"
"set_key(key) -- sets the key for further encryption or decryptions\n"
"encrypt(val) -- encrypts a 128-bit value, returns a 128-bit encrypted value.\n"
"decrypt(val) -- decrypts a 128-bit value, returns a 128-bit decrypted value.\n"
"xor_block(val,mask) -- exclusive-ors a 128-bit value with a 128-bit mask.\n"
"cfb_salt(salt) -- initializes CFB mode counter.\n"
"cfb_encrypt(string) -- returns a string of same length encrypted in CFB mode.\n"
"cfb_decrypt(string) -- returns a string of same length decrypted in CFB mode.\n"
"cfb_encrypt128(string) -- returns a string of same length encrypted in CFB128 mode.\n"
"cfb_decrypt128(string) -- returns a string of same length encrypted in CFB128 mode.\n";

statichere PyTypeObject TwoFishType = {
  PyObject_HEAD_INIT(&PyType_Type)
    0,           /* ob_size */
    "_twofishtype",  /* type name */
    sizeof(TwoFishObject),   /* tp_size */
    0,   /* tp_itemsize */
    /* methods */
    (destructor)twofish_dealloc, /* tp_dealloc*/
    0,             /* tp_print */
    (getattrfunc)twofish_getattr,  /* tp_getattr*/
    0,     /* tp_sertattr*/
    0,     /* tp_compare */
    0,   /* tp_repr */
    0,   /* tp_as_number */
    0, /* tp_as_sequence */
    0,  /* tp_as_mapping*/
    0,  /* tp_hash */
    0, /* tp_call */
    0, /* tp_str */
    0, /* tp_getattro */
    0, /* tp_set_attro */
    0, /* tp_as_buffer */
    0, /* tp_xxx4 */
    TwoFishType_doc  /* tp_doc */
    };

/* List of functions exported by this module */

static PyMethodDef twofish_functions[] = {
  {"new", (PyCFunction)newTwoFishObject, 1,new_doc},
  {NULL, NULL, 0, NULL}
};

/* initialize this module. */
DL_EXPORT(void)
init_twofish() 
{
  PyObject *m, *d;
  m=Py_InitModule3("_twofish",twofish_functions,module_doc);
  d=PyModule_GetDict(m);
  PyDict_SetItemString(d,"_twofish_type",(PyObject *)&TwoFishType);
  /* and we don't check the error here... caller has to. */
}

