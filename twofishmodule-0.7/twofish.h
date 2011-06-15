/* 1. Standard types for AES cryptography source code               */

#ifndef TWOFISH_H
#define TWOFISH_H 1

typedef unsigned char   u1byte; /* an 8 bit unsigned character type */
typedef unsigned short  u2byte; /* a 16 bit unsigned integer type   */
typedef unsigned int   u4byte; /* a 32 bit unsigned integer type   */

typedef signed char     s1byte; /* an 8 bit signed character type   */
typedef signed short    s2byte; /* a 16 bit signed integer type     */
typedef signed int     s4byte; /* a 32 bit signed integer type     */

/* 2. Standard interface for AES cryptographic routines             */

/* These are all based on 32 bit unsigned values and may require    */
/* endian conversion for big-endian architectures                   */

/***FIXME*** Fix this so that it's not hard-wired! */

#ifndef LITTLE_ENDIAN
#define	LITTLE_ENDIAN
#endif

/* Now for the twofish context: */
struct twofish_context {
  u4byte k_len;
  u4byte l_key[40];
  u4byte s_key[4];
  u4byte  qt_gen;
  u1byte  q_tab[2][256];
  u4byte mt_gen;
  u4byte m_tab[4][256];
  u4byte key_gen;
};



#ifdef  __cplusplus
    extern "C"
    {
#endif

    char *cipher_name(void);
    void set_key(struct twofish_context *ctx,const u1byte in_key[], const u4byte key_len);
    void encrypt(struct twofish_context *ctx,const u1byte in_blk[16], u1byte out_blk[16]);
    void decrypt(struct twofish_context *ctx,const u1byte in_blk[16], u1byte out_blk[16]);
  struct twofish_context *init_ctx(void); /* return a new context. */
#ifdef  __cplusplus
    };
#endif

/* 3. Basic macros for speeding up generic operations               */

/* Circular rotate of 32 bit values                                 */

#ifdef _MSC_VER

#  include <stdlib.h>
#  pragma intrinsic(_lrotr,_lrotl)
#  define rotr(x,n) _lrotr(x,n)
#  define rotl(x,n) _lrotl(x,n)

#else

#define rotr(x,n)   (((x) >> ((int)(n))) | ((x) << (32 - (int)(n))))
#define rotl(x,n)   (((x) << ((int)(n))) | ((x) >> (32 - (int)(n))))

#endif

/* Invert byte order in a 32 bit variable                           */

#define bswap(x)    (rotl(x, 8) & 0x00ff00ff | rotr(x, 8) & 0xff00ff00)

/* Extract byte from a 32 bit quantity (little endian notation)     */

#define byte(x,n)   ((u1byte)((x) >> (8 * n)))

/* Input or output a 32 bit word in machine order					*/

#ifdef	LITTLE_ENDIAN

#define	u4byte_in(x)		(*(u4byte*)(x))
#define	u4byte_out(x, v)	(*(u4byte*)(x) = (v))

#else

#define	u4byte_in(x)		bswap(*(u4byte)(x))
#define	u4byte_out(x, v)	(*(u4byte*)(x) = bswap(v))

#endif


#endif
