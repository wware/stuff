/* Hyper Text Query Server
   23-DEC-94 George J. Carrette.

 This is a SIOD main program with additional command-line processing
 functionality.

*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "siod.h"

static char *siod_argv[] = {
  "siod",
  "-h100000:10",
  "-g0",
  "-o1000",
  "-s200000",
  "-n2048"};

static void init_htqs_version(void)
{setvar(cintern("*htqs-version*"),
	cintern("$Id: siod.c,v 1.6 1996/03/10 02:31:10 gjc Exp $"),
	NIL);}

static LISP cgi_main(LISP result)
{if (CONSP(result) && TYPEP(car(result),tc_string))
   {put_st("Status: 500 Server Error (Application)\n");
    put_st("Content-type: text/html\n\n");
    put_st("<HTML><HEAD><TITLE>Server Error (Application)</TITLE></HEAD>\n");
    put_st("<BODY><H1>Server Error (Application)</H1>\n");
    put_st("An application on this server has encountered an error\n");
    put_st("which prevents it from fulfilling your request.");
    put_st("<P><PRE><B>Error Message:</B> ");
    lprint(car(result),NIL);
    if NNULLP(cdr(result))
      {put_st("\n");
       lprint(cdr(result),NIL);}
    put_st("</PRE></BODY></HTML>\n");
    err("cgi-main",NIL);}
 return(NIL);}

main(int argc,char **argv
#if defined(unix) || defined(vms)
     ,char **env
#endif
     )
{int j,retval,iargc,mainflag = 0,text_plain_flag = 0;
 char *iargv[2],*start,*end;
 LISP l;
 process_cla(sizeof(siod_argv)/sizeof(char *),siod_argv,1);
 iargv[0] = "";
 for(iargc=0,j=1;j<argc; ++j)
   if (*(start = argv[j]) == '-')
     {while(*start)
	{if (!(end = strstr(start,",-"))) end = &start[strlen(start)];
	 iargv[1] = (char *) malloc(end-start+1);
	 memcpy(iargv[1],start,end-start);
	 iargv[1][end-start] = 0;
	 if ((strncmp(iargv[1],"-v",2) == 0) &&
	     (atol(&iargv[1][2]) > 0) &&
	     (iargv[1][2] != '0'))
	   {printf("Content-type: text/plain\r\n\r\n");
	    text_plain_flag = 1;}
	 if ((strncmp(iargv[1],"-m",2) == 0))
	   mainflag = atol(&iargv[1][2]);
	 else
	   process_cla(2,iargv,1);
	 /* Note: Not doing free(iargv[1]); */
	 start = (*end) ? end+1 : end;}}
 else
   ++iargc;
 print_welcome();
 print_hs_1();
 init_storage();
 for(l=NIL,j=0;j<argc;++j)
   l = cons(strcons(strlen(argv[j]),argv[j]),l);
 setvar(cintern("*args*"),nreverse(l),NIL);
 l = NIL;
#if defined(unix) || defined(vms)
 for(l=NIL,j=0;env[j];++j)
   l = cons(strcons(strlen(env[j]),env[j]),l);
 setvar(cintern("*env*"),nreverse(l),NIL);
 l = NIL;
#endif
 init_subrs();
 init_trace();
 init_slibu();
 init_subr_1("__cgi-main",cgi_main);
 init_htqs_version();

 if (iargc == 0)
   retval = repl_driver(1,1,NULL);
 else
   {for(j=1;j<(((mainflag >= 2) && (argc > 3)) ? 3 : argc);++j)
      if (argv[j][0] != '-')
	{retval = htqs_arg(argv[j]);
	 if (retval != 0) break;}
    if (mainflag)
      retval = htqs_arg(((mainflag > 2) && !text_plain_flag)
			? "(__cgi-main (*catch 'errobj (main))))"
			: "(main)");}
 if (siod_verbose_check(2))
   printf("EXIT\n");
 exit(retval);}

int htqs_arg(char *value)
{char tmpbuff[256];
 if ((strcmp(value,"(repl)") == 0) ||
     (strcmp(value,"repl") == 0))
   return(repl_driver(1,1,NULL));
 else if (!strchr(value,'('))
   {sprintf(tmpbuff,"(require \"%s\")",value);
    return(repl_c_string(tmpbuff,0,0,0));}
 else
   return(repl_c_string(value,0,0,0));}

