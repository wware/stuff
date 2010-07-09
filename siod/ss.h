/* $Id: ss.h,v 1.2 1995/12/06 15:07:02 gjc Exp $ */

struct sock_stream {
  int sd;
  int icnt;
  unsigned char	*iptr;
  unsigned char *ibase;
  int ocnt;
  unsigned char	*optr;
  unsigned char *obase;
  int bufsiz;};

#define SS_GETC(p) (--(p)->icnt < 0 ? ss_filbuf(p) : (int) *(p)->iptr++)

#define SS_PUTC(c,p) \
  (--(p)->ocnt < 0 ? ss_flsbuf((int)(c),(p)) : \
   (int)(*(p)->optr++ = (unsigned char)(c)))


LISP s_open(LISP lhost,LISP lport,LISP aflag);
LISP s_close(LISP s);
int ss_filbuf(struct sock_stream *ss);
int ss_flsbuf(int c,struct sock_stream *ss);
struct sock_stream *get_ss(LISP s,long openchk);
void ss_force(struct sock_stream *ss);
LISP s_accept(LISP s);
