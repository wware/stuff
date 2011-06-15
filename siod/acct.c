#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <utmp.h>
#if defined(__osf__)
#include <sys/acct.h>
#endif
#include "siod.h"

static void init_acct_version(void)
{setvar(cintern("*acct-version*"),
	cintern("$Id: acct.c,v 1.5 1996/03/11 22:31:43 gjc Exp $"),
	NIL);}

/* decode various accounting structures */

/* I could not find any include file for this structure, only documentation
   for it in the "man acct" */

struct tacct
{uid_t ta_uid;
 char ta_name[32];
 double ta_cpu[2];
 double ta_kcore[2];
 double ta_io[2];
 double ta_rw[2];
 double ta_con[2];
 double ta_du;
 long ta_qsys;
 double ta_fee;
 long ta_pc;
 unsigned short ta_sc;
 unsigned short ta_dc;};


LISP decode_utmp(struct utmp *p)
{return(symalist("user",strcons(SAFE_STRLEN(p->ut_user),p->ut_user),
		 "id",strcons(SAFE_STRLEN(p->ut_id),p->ut_id),
		 "line",strcons(SAFE_STRLEN(p->ut_line),p->ut_line),
		 "type",
#ifdef EMPTY
		 (p->ut_type == EMPTY) ? cintern("EMPTY") :
#endif
		 (p->ut_type == RUN_LVL) ? cintern("RUN_LVL") :
		 (p->ut_type == BOOT_TIME) ? cintern("BOOT_TIME") :
		 (p->ut_type == OLD_TIME) ? cintern("OLD_TIME") :
		 (p->ut_type == NEW_TIME) ? cintern("NEW_TIME") :
		 (p->ut_type == INIT_PROCESS) ? cintern("INIT_PROCESS") :
		 (p->ut_type == LOGIN_PROCESS) ? cintern("LOGIN_PROCESS") :
		 (p->ut_type == USER_PROCESS) ? cintern("USER_PROCESS") :
		 (p->ut_type == DEAD_PROCESS) ? cintern("DEAD_PROCESS") :
#ifdef ACCOUNTING
		 (p->ut_type == ACCOUNTING) ? cintern("ACCOUNTING") :
#endif
		 flocons(p->ut_type),
	         "pid",flocons(p->ut_pid),
#if defined(__osf__)
	         "termination",flocons(p->ut_exit.e_termination),
	         "exit",flocons(p->ut_exit.e_exit),
#endif
	         "ut_time",flocons(p->ut_time),
		 "host",strcons(SAFE_STRLEN(p->ut_host),p->ut_host),
		 NULL));}

LISP lgetutent(void)
{struct utmp *p;
 long iflag;
 iflag = no_interrupt(1);
 p = getutent();
 no_interrupt(iflag);
 return((p) ? decode_utmp(p) : NIL);}

LISP lsetutent(void)
{long iflag;
 iflag = no_interrupt(1);
 setutent();
 no_interrupt(iflag);
 return(NIL);}

LISP lendutent(void)
{long iflag;
 iflag = no_interrupt(1);
 endutent();
 no_interrupt(iflag);
 return(NIL);}

LISP lutmpname(LISP name)
{long iflag;
 iflag = no_interrupt(1);
 utmpname(get_c_string(name));
 no_interrupt(iflag);
 return(NIL);}

#if defined(__osf__)

LISP decode_acct(struct acct *p)
{LISP flags = NIL;
 if (p->ac_flag & AFORK) flags = cons(cintern("FORK"),flags);
 if (p->ac_flag & ASU) flags = cons(cintern("SU"),flags);
 if (p->ac_flag & ACOMPAT) flags = cons(cintern("COMPAT"),flags);
 if (p->ac_flag & ACORE) flags = cons(cintern("CORE"),flags);
 if (p->ac_flag & AXSIG) flags = cons(cintern("XSIG"),flags);
 if (p->ac_flag & ACCTF) flags = cons(flocons((p->ac_flag & ACCTF) >> 6),
				      flags);
 return(symalist(
		 "comm",strcons(SAFE_STRLEN(p->ac_comm),p->ac_comm),
		 "io",flocons(expacct(p->ac_io) / AHZ),
		 "utime",flocons(expacct(p->ac_utime) / AHZ),
		 "stime",flocons(expacct(p->ac_stime) / AHZ),
		 "etime",flocons(expacct(p->ac_etime) / AHZ),
		 "btime",flocons(p->ac_btime),
		 "uid",flocons(p->ac_uid),
		 "gid",flocons(p->ac_gid),
		 "mem",flocons(p->ac_mem),
		 "rw",flocons(expacct(p->ac_rw)),
		 "tty",listn(2,
			     flocons(major(p->ac_tty)),
			     flocons(minor(p->ac_tty))),
		 "flag",flags,
		 "stat",flocons(p->ac_stat),
		 NULL));}

LISP ldecode_acct(LISP l)
{char *buffer;
 long n;
 buffer = get_c_string_dim(l,&n);
 if (n != sizeof(struct acct))
   err("not correct size for struct acct",l);
 return(decode_acct((struct acct *) buffer));}

LISP decode_tacct(struct tacct *p)
{return(symalist("uid",flocons(p->ta_uid),
		 "name",strcons(SAFE_STRLEN(p->ta_name),p->ta_name),
		 "cpu",listn(2,
			     flocons(p->ta_cpu[0]),
			     flocons(p->ta_cpu[1])),
		 "kcore",listn(2,
			       flocons(p->ta_kcore[0]),
			       flocons(p->ta_kcore[1])),
		 "io",listn(2,
			    flocons(p->ta_io[0]),
			    flocons(p->ta_io[1])),
		 "rw",listn(2,
			    flocons(p->ta_rw[0]),
			    flocons(p->ta_rw[1])),
		 "con",listn(2,
			     flocons(p->ta_con[0]),
			     flocons(p->ta_con[1])),
		 "du",flocons(p->ta_du),
		 "qsys",flocons(p->ta_qsys),
		 "fee",flocons(p->ta_fee),
		 "pc",flocons(p->ta_pc),
		 "sc",flocons(p->ta_sc),
		 "dc",flocons(p->ta_dc),
		 NULL));}

LISP ldecode_tacct(LISP l)
{char *buffer;
 long n;
 buffer = get_c_string_dim(l,&n);
 if (n != sizeof(struct tacct))
   err("not correct size for struct tacct",l);
 return(decode_tacct((struct tacct *) buffer));}

#endif

void init_acct(void)
{setvar(cintern("UTMP_FILE"),strcons(strlen(UTMP_FILE),UTMP_FILE),NIL);
 setvar(cintern("WTMP_FILE"),strcons(strlen(WTMP_FILE),WTMP_FILE),NIL);
 init_subr_0("getutent",lgetutent);
 init_subr_0("setutent",lsetutent);
 init_subr_0("endutent",lendutent);
 init_subr_1("utmpname",lutmpname);
#if defined(__osf__)
 setvar(cintern("SIZEOF_ACCT"),flocons(sizeof(struct acct)),NIL);
 init_subr_1("decode_acct",ldecode_acct);
 setvar(cintern("SIZEOF_TACCT"),flocons(sizeof(struct tacct)),NIL);
 init_subr_1("decode_tacct",ldecode_tacct);
#endif
 init_acct_version();}

