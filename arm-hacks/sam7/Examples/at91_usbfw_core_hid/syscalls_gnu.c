// minimal syscalls - collected and modified by Martin Thomas 11/06
// most of this from newlib-lpc

#include <stdlib.h>
#include <stdio.h>
#include <reent.h>
#include <sys/stat.h>

extern int my_fputc(int out, FILE *f);

_ssize_t _read_r(
    struct _reent *r, 
    int file, 
    void *ptr, 
    size_t len)
{
	return (_ssize_t)0;
}

_ssize_t _write_r (
    struct _reent *r, 
    int file, 
    const void *ptr, 
    size_t len)
{
	size_t todo;
	const unsigned char *p;

	todo = len;
	p = ptr;
	
	for( ; todo != 0; todo--) {
		my_fputc(*p++, stdout);
	}
	
	return (_ssize_t)len;			/* Number of bytes written.	*/
}


int _close_r(
    struct _reent *r, 
    int file)
{
	return 0;
}

_off_t _lseek_r(
    struct _reent *r, 
    int file, 
    _off_t ptr, 
    int dir)
{
	return (_off_t)0;	/*  Always indicate we are at file beginning.  */
}

int _fstat_r(
    struct _reent *r, 
    int file, 
    struct stat *st)
{
	/*  Always set as character device.         */
	st->st_mode = S_IFCHR;
	/* assigned to strong type with implicit    */
	/* signed/unsigned conversion.  Required by */
	/* newlib.                                  */

	return 0;
}

int isatty(int file); /* avoid warning */

int isatty(int file)
{
	return 1;
}


/*  end is set in the linker command    */
/* file and is the end of statically    */
/* allocated data (thus start of heap). */
extern char end[];     

static char *heap_ptr;	  /* Points to current end of the heap.	*/

/************************** _sbrk_r *************************************
 * Support function. Adjusts end of heap to provide more memory to
 * memory allocator. Simple and dumb with no sanity checks.

 *  struct _reent *r -- re-entrancy structure, used by newlib to
 *                      support multiple threads of operation.
 *  ptrdiff_t nbytes -- number of bytes to add.
 *                      Returns pointer to start of new heap area.
 *
 *  Note:  This implementation is not thread safe (despite taking a
 *         _reent structure as a parameter).
 *         Since _s_r is not used in the current implementation, 
 *         the following messages must be suppressed.
 */
void * _sbrk_r(
    struct _reent *_s_r, 
    ptrdiff_t nbytes)
{
	char  *base;		/*  errno should be set to  ENOMEM on error  */

	if (!heap_ptr) {	/*  Initialize if first time through.  */
		heap_ptr = end;
	}
	base = heap_ptr;	/*  Point to end of heap.  */
	heap_ptr += nbytes;	/*  Increase heap.  */
	
	return base;		/*  Return pointer to start of new heap area.  */
}
