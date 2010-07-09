#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ndbm.h>
#include "siod.h"
#if defined(sun) || defined(hpux) || defined(linux) || defined(sgi)
#include <sys/types.h>
#endif


static void init_ndbm_version(void)
{setvar(cintern("*ndbm-version*"),
	cintern("$Id: ndbm.c,v 1.13 1996/03/14 17:51:53 gjc Exp $"),
	NIL);}

long tc_dbm = 0;

DBM *get_DBM(LISP ptr,int errflg)
{DBM *p;
 if (NTYPEP(ptr,tc_dbm))
   err("not a DBM",ptr);
 if (p = (DBM *) ptr->storage_as.string.data)
   return(p);
 else if (errflg)
   err("DBM closed",ptr);
 return(NULL);}

LISP ldbm_open(LISP lfname,LISP lflags,LISP lmode)
{mode_t mode;
 int flags;
 char *fname;
 long iflag;
 LISP result;
 DBM *db;
 fname = get_c_string(lfname);
 flags = get_c_long(FLONUMP(lflags) ? lflags : encode_open_flags(lflags));
 mode = get_c_long(FLONUMP(lmode) ? lmode : encode_st_mode(lmode));
 result = cons(NIL,NIL);
 iflag = no_interrupt(1);
 if (!(db = dbm_open(fname,flags,mode)))
   return(err("dbm_open",llast_c_errmsg(-1)));
 result->type = tc_dbm;
 result->storage_as.string.data = (char *) db;
 no_interrupt(iflag);
 return(result);}

LISP ldbm_close(LISP ldbm)
{long iflag;
 DBM *db;
 db = get_DBM(ldbm,1);
 iflag = no_interrupt(1);
 dbm_close(db);
 ldbm->storage_as.string.data = NULL;
 no_interrupt(iflag);
 return(NIL);}

static LISP cons_from_datum(datum *dat)
{LISP result;
 if (!dat->dptr)
   return(NIL);
 else
   {result = arcons(tc_byte_array,dat->dsize,0);
    memcpy(result->storage_as.string.data,dat->dptr,dat->dsize);
    return(result);}}

LISP ldbm_fetch(LISP ldbm,LISP lkey)
{long iflag;
 DBM *db;
 char *key;
 long keysize;
 datum dat1,dat2;
 db = get_DBM(ldbm,1);
 key = get_c_string_dim(lkey,&keysize);
 dat1.dptr = key;
 dat1.dsize = keysize;
 iflag = no_interrupt(1);
 dat2 = dbm_fetch(db,dat1);
 no_interrupt(iflag);
 return(cons_from_datum(&dat2));}

LISP ldbm_firstkey(LISP ldbm)
{long iflag;
 DBM *db;
 datum dat;
 db = get_DBM(ldbm,1);
 iflag = no_interrupt(1);
 dat = dbm_firstkey(db);
 no_interrupt(iflag);
 return(cons_from_datum(&dat));}

LISP ldbm_nextkey(LISP ldbm)
{long iflag;
 DBM *db;
 datum dat;
 db = get_DBM(ldbm,1);
 iflag = no_interrupt(1);
 dat = dbm_nextkey(db);
 no_interrupt(iflag);
 return(cons_from_datum(&dat));}

LISP ldbm_delete(LISP ldbm,LISP lkey)
{long iflag,status;
 DBM *db;
 char *key;
 long keysize;
 datum dat;
 db = get_DBM(ldbm,1);
 key = get_c_string_dim(lkey,&keysize);
 dat.dptr = key;
 dat.dsize = keysize;
 iflag = no_interrupt(1);
 status = dbm_delete(db,dat);
 no_interrupt(iflag);
 if (status)
   err("dbm_delete",llast_c_errmsg(-1));
 return(NIL);}

LISP ldbm_store(LISP ldb,LISP lkey,LISP ldata,LISP lflags)
{long iflag,status;
 DBM *db;
 char *key,*data;
 long keysize,datasize;
 datum dat1,dat2;
 int flags;
 db = get_DBM(ldb,1);
 key = get_c_string_dim(lkey,&keysize);
 data = get_c_string_dim(ldata,&datasize);
 flags = NULLP(lflags) ? 0 : get_c_long(lflags);
 dat1.dptr = key;
 dat1.dsize = keysize;
 dat2.dptr = data;
 dat2.dsize = datasize;
 iflag = no_interrupt(1);
 status = dbm_store(db,dat1,dat2,flags);
 no_interrupt(iflag);
 if ((status == 1) && (flags == DBM_INSERT))
   return(NIL);
 else if (status)
   err("dbm_store",llast_c_errmsg(-1));
 else
   return(a_true_value());}

LISP ldbm_dirfno(LISP ldb)
{return(flocons(dbm_dirfno(get_DBM(ldb,1))));}

LISP ldbm_pagfno(LISP ldb)
{return(flocons(dbm_pagfno(get_DBM(ldb,1))));}

void dbm_gc_free(LISP ptr)
{DBM *p;
 if (p = get_DBM(ptr,0))
   dbm_close(p);
 ptr->storage_as.string.data = NULL;}

void dbm_prin1(LISP ptr,struct gen_printio *f)
{char buff[256];
 DBM *p;
 p = get_DBM(ptr,0);
 sprintf(buff,"#<DBM %p>",p);
 gput_st(f,buff);}

LISP ldbm_rdonly(LISP ldb)
{return((dbm_rdonly(get_DBM(ldb,1))) ? a_true_value() : NIL);}

LISP ldbm_error(LISP ldb)
{return((dbm_error(get_DBM(ldb,1))) ? a_true_value() : NIL);}

void init_ndbm(void)
{long j;
 tc_dbm = allocate_user_tc();
 set_gc_hooks(tc_dbm,
	      NULL,
	      NULL,
	      NULL,
	      dbm_gc_free,
	      &j);
 set_print_hooks(tc_dbm,dbm_prin1);
 init_subr_3("dbm_open",ldbm_open);
 init_subr_1("dbm_close",ldbm_close);
 init_subr_2("dbm_fetch",ldbm_fetch);
 init_subr_1("dbm_firstkey",ldbm_firstkey);
 init_subr_1("dbm_nextkey",ldbm_nextkey);
 init_subr_2("dbm_delete",ldbm_delete);
 init_subr_4("dbm_store",ldbm_store);
 setvar(cintern("DBM_INSERT"),flocons(DBM_INSERT),NIL);
 setvar(cintern("DBM_REPLACE"),flocons(DBM_REPLACE),NIL);
#ifdef PBLKSIZ
 setvar(cintern("PBLKSIZ"),flocons(PBLKSIZ),NIL);
#endif
#ifdef DBLKSIZ
 setvar(cintern("DBLKSIZ"),flocons(DBLKSIZ),NIL);
#endif
 init_subr_1("dbm_dirfno",ldbm_dirfno);
 init_subr_1("dbm_pagfno",ldbm_pagfno);
 init_subr_1("dbm_rdonly",ldbm_rdonly);
 init_subr_1("dbm_error",ldbm_error);
 init_ndbm_version();}

