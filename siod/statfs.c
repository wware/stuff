/*-*-mode:c-*-*/

/* $Id: statfs.c,v 1.2 1996/06/11 12:40:04 gjc Exp $ */

#include <stdio.h>
#include <sys/mount.h>
#include <sys/fs_types.h>
#include <fstab.h>
#include "siod.h"

#define MNT_NUMTYPES	128
/*
 * definition of mnt_names array moved to usr/ccs/lib/libc/getvfs.c
 */
extern char *mnt_names[];

LISP lstatfs(LISP path)
{long iflag;
 struct statfs s;
 iflag = no_interrupt(1);
 if (statfs(get_c_string(path),&s,sizeof(s)))
   return(err("statfs",llast_c_errmsg(-1)));
 no_interrupt(iflag);
 return(symalist("type",(((s.f_type >= 0) && (s.f_type < MNT_NUMTYPES) &&
			  mnt_names[s.f_type])
			 ? rintern(mnt_names[s.f_type])
			 : flocons(s.f_type)),
		 "bsize",flocons(s.f_bsize),
		 "blocks",flocons(s.f_blocks),
		 "bfree",flocons(s.f_bfree),
		 "bavail",flocons(s.f_bavail),
		 "files",flocons(s.f_files),
		 "ffree",flocons(s.f_ffree),
		 "mntonname",strcons(-1,s.f_mntonname),
		 "mntfromname",strcons(-1,s.f_mntfromname),
		 NULL));}


static LISP decode_fstab(struct fstab *p)
{if (p)
   return(symalist("spec",strcons(-1,p->fs_spec),
		   "file",strcons(-1,p->fs_file),
		   "type",strcons(-1,p->fs_type),
		   "freq",flocons(p->fs_freq),
		   "passno",flocons(p->fs_passno),
		   "vfstype",rintern(p->fs_vfstype),
		   "mntops",strcons(-1,p->fs_mntops),
		   NULL));
 else
   return(NIL);}

LISP lgetfsent(void)
{long iflag;
 LISP result;
 iflag = no_interrupt(1);
 result = decode_fstab(getfsent());
 no_interrupt(iflag);
 return(result);}

LISP lsetfsent(void)
{long iflag;
 LISP result;
 iflag = no_interrupt(1);
 result = flocons(setfsent());
 no_interrupt(iflag);
 return(result);}

LISP lendfsent(void)
{long iflag;
 iflag = no_interrupt(1);
 endfsent();
 no_interrupt(iflag);
 return(NIL);}

void init_statfs(void)
{init_subr_1("statfs",lstatfs);
 init_subr_0("getfsent",lgetfsent);
 init_subr_0("setfsent",lsetfsent);
 init_subr_0("endfsent",lendfsent);}
