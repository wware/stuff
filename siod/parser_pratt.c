/*-*-mode:c-*-*/

/*

;; Based on a theory of parsing presented in:                       
;;                                                                      
;;  Pratt, Vaughan R., ``Top Down Operator Precedence,''         
;;  ACM Symposium on Principles of Programming Languages         
;;  Boston, MA; October, 1973.                                   

*/

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>

#include "siod.h"

static LISP sym_whitespace = NIL;
static LISP sym_regular = NIL;
static LISP sym_string_delim = NIL;
static LISP sym_nl_whitespace = NIL;
static LISP sym_back_slash = NIL;

static LISP err_token_overflow(void)
{err("token_overflow",NIL);}

LISP pratt_read_token(LISP buffer,LISP chars,LISP stream)
{LISP c,tail;
 char *tk;
 long j=0,tkdim;
 tk = get_c_string_dim(buffer,&tkdim);
 while(1)
   if NULLP(c = lgetc(stream))
     return(get_eof_val());
   else if EQ(sym_nl_whitespace,tail = aref1(chars,c))
     while (NNULLP(c = lgetc(stream)) && (get_c_long(c) != '\n'));
   else if NEQ(sym_whitespace,tail)
     break;
 if EQ(sym_string_delim,tail)
   while(NNULLP(c = lgetc(stream)))
     if EQ(sym_back_slash,aref1(chars,c))
       {switch(get_c_long(c = lgetc(stream)))
	  {case 'n':
	     c = flocons('\n');
	     break;
	   case 't':
	     c = flocons('\t');}
	if (j<tkdim)
	  tk[j++] = get_c_long(c);
	else
	  err_token_overflow();}
     else if EQ(sym_string_delim,aref1(chars,c))
       return(strcons(j,tk));
     else if (j<tkdim)
       tk[j++] = get_c_long(c);
     else
       err_token_overflow();
 if EQ(tail,sym_back_slash)
   c = lgetc(stream);
 if (j<tkdim)
   tk[j++] = get_c_long(c);
 else
   err_token_overflow();
 if NULLP(tail)
   return(lreadtk(tk,j));
 else if CONSP(tail)
   while(1)
     if NULLP(c = lgetc(stream))
       return(lreadtk(tk,j));
     else if NNULLP(tail = assq(c,tail))
       if (j<tkdim)
	 tk[j++] = get_c_long(c);
       else
	 err_token_overflow();
     else
       {if NEQ(sym_whitespace,aref1(chars,c)) lungetc(c,stream);
	return(lreadtk(tk,j));}
 while(1)
   if NULLP(c = lgetc(stream))
     return(lreadtk(tk,j));
   else if EQ(sym_back_slash,tail = aref1(chars,c))
     {c = lgetc(stream);
      if (j<tkdim)
	tk[j++] = get_c_long(c);
      else
	err_token_overflow();}
   else if NEQ(sym_regular,tail)
     {if NEQ(sym_whitespace,tail) lungetc(c,stream);
      return(lreadtk(tk,j));}
   else if (j<tkdim)
     tk[j++] = get_c_long(c);
   else
     err_token_overflow();
 return(NIL);}
  
LISP init_parser_pratt(void)
{init_subr_3("pratt_read_token",pratt_read_token);
 gc_protect_sym(&sym_whitespace,"whitespace");
 gc_protect_sym(&sym_regular,"regular");
 gc_protect_sym(&sym_string_delim,"string-delim");
 gc_protect_sym(&sym_nl_whitespace,"nl-whitespace");
 gc_protect_sym(&sym_back_slash,"back-slash");}
