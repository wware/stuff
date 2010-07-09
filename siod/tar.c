#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "siod.h"

static void init_tar_version(void)
{setvar(cintern("*tar-version*"),
	cintern("$Id: tar.c,v 1.3 1995/12/06 14:04:40 gjc Exp $"),
	NIL);}


struct tar_header
{char name[100];
 char mode[8];
 char gid[8];
 char uid[8];
 char size[12];
 char mtime[12];
 char checksum[8];
 char typeflag;
 char linkname[100];
 /* posix adds more data here, which would be 0 in bsd */
 char magic[6];
 char version[2];
 char uname[32];
 char gname[32];
 char devmajor[8];
 char devminor[8];
 char prefix[155];
 char end_pad[12];};

static long safe_atol8(const char *ptr,size_t len)
{long x = 0;
 int c,j;
 for(j=0;j<len && (c = ptr[j]) && isdigit(c); ++j)
   x = x * 8 + c - '0';
 return(x);}

#define SAFE_ATOL8(_buff) (safe_atol8((_buff),sizeof(_buff)))

LISP decode_tartype(char c)
{switch(c)
   {case 0:
    case '0':
      return(cintern("REG"));
    case '1':
      return(cintern("LNK"));
    case '2':
      return(cintern("SYM"));
    case '3':
      return(cintern("CHR"));
    case '4':
      return(cintern("BLK"));
    case '5':
      return(cintern("DIR"));
    case '6':
      return(cintern("FIFO"));
    default:
      return(NIL);}}

static struct tar_header *get_tar_header(LISP bytes)
{long n;
 struct tar_header *h;
 h = (struct tar_header *) get_c_string_dim(bytes,&n);
 if (n < sizeof(struct tar_header)) err("too small for tar header",bytes);
 return(h);}

LISP decode_tar_header(LISP bytes)
{struct tar_header *h = get_tar_header(bytes);
 return(listn(16,
	      cons(cintern("name"),
		   strcons(SAFE_STRLEN(h->name),h->name)),
	      cons(cintern("mode"),
		   flocons(SAFE_ATOL8(h->mode))),
	      cons(cintern("gid"),
		   flocons(SAFE_ATOL8(h->gid))),
	      cons(cintern("uid"),
		   flocons(SAFE_ATOL8(h->uid))),
	      cons(cintern("size"),
		   flocons(SAFE_ATOL8(h->size))),
	      cons(cintern("mtime"),
		   flocons(SAFE_ATOL8(h->mtime))),
	      cons(cintern("checksum"),
		   flocons(SAFE_ATOL8(h->checksum))),
	      cons(cintern("type"),
		   decode_tartype(h->typeflag)),
	      cons(cintern("linkname"),
		   strcons(SAFE_STRLEN(h->linkname),h->linkname)),
	      cons(cintern("magic"),
		   strcons(SAFE_STRLEN(h->magic),h->magic)),
	      cons(cintern("version"),
		   strcons(SAFE_STRLEN(h->version),h->version)),
	      cons(cintern("uname"),
		   strcons(SAFE_STRLEN(h->uname),h->uname)),
	      cons(cintern("gname"),
		   strcons(SAFE_STRLEN(h->gname),h->gname)),
	      cons(cintern("devmajor"),
		   strcons(SAFE_STRLEN(h->devmajor),h->devmajor)),
	      cons(cintern("devminor"),
		   strcons(SAFE_STRLEN(h->devminor),h->devminor)),
	      cons(cintern("prefix"),
		   strcons(SAFE_STRLEN(h->prefix),h->prefix))));}

static unsigned long checksum(void *start,
			    void *end)
{unsigned char *ptr;
 unsigned long sum = 0;
 for(ptr=(unsigned char *)start;ptr<(unsigned char *)end;++ptr)
   sum += *ptr;
 return(sum);}

LISP checksum_tar_header(LISP bytes,LISP whole)
{struct tar_header *h = get_tar_header(bytes);
 if NNULLP(whole)
   return(flocons(checksum(&h->name[0],
			   &h->end_pad[sizeof(h->end_pad)])));
 else
   return(flocons(checksum(&h->name[0],&h->checksum[0]) +
		  ' ' * sizeof(h->checksum) +
		  checksum(&h->typeflag,
			   &h->end_pad[sizeof(h->end_pad)])));}

void init_tar(void)
{init_subr_1("decode-tar-header",decode_tar_header);
 init_subr_2("checksum-tar-header",checksum_tar_header);
 setvar(cintern("*tar-header-size*"),
	flocons(sizeof(struct tar_header)),
	NIL);
 init_tar_version();}

