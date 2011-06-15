#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "gd.h"
#include "siod.h"

static void init_gd_version(void)
{setvar(cintern("*gd-version*"),
	cintern("$Id: gd.c,v 1.6 1996/03/10 02:31:10 gjc Exp $"),
	NIL);}

long tc_gdimage = 0;
long tc_gdfont = 0;
long tc_gdpoint = 0;

extern gdFontPtr gdFontGiant;
extern gdFontPtr gdFontLarge;
extern gdFontPtr gdFontMediumBold;
extern gdFontPtr gdFontSmall;
extern gdFontPtr gdFontTiny;

LISP lgdImageCreate(LISP sx,LISP sy)
{LISP result;
 long iflag;
 result = cons(NIL,NIL);
 result->type = tc_gdimage;
 iflag = no_interrupt(1);
 result->storage_as.string.data = (char *) gdImageCreate(get_c_long(sx),
							 get_c_long(sy));
 no_interrupt(iflag);
 return(result);}

LISP lgdImageCreateFromGif(LISP f)
{LISP result;
 long iflag;
 result = cons(NIL,NIL);
 result->type = tc_gdimage;
 iflag = no_interrupt(1);
 result->storage_as.string.data =
   (char *) gdImageCreateFromGif(get_c_file(f,NULL));
 no_interrupt(iflag);
 return(result);}

LISP lgdImageCreateFromXbm(LISP f)
{LISP result;
 long iflag;
 result = cons(NIL,NIL);
 result->type = tc_gdimage;
 iflag = no_interrupt(1);
 result->storage_as.string.data =
   (char *) gdImageCreateFromXbm(get_c_file(f,NULL));
 no_interrupt(iflag);
 return(result);}


gdImagePtr get_gdImagePtr(LISP ptr)
{gdImagePtr im;
 if (NTYPEP(ptr,tc_gdimage))
   err("not a gdImage",ptr);
 if (!(im = (gdImagePtr) ptr->storage_as.string.data))
   err("gd Image deallocated",ptr);
 return(im);}

LISP lcgdFontCreate(gdFontPtr font)
{LISP result;
 long iflag;
 result = cons(NIL,NIL);
 result->type = tc_gdfont;
 iflag = no_interrupt(1);
 result->storage_as.string.data = (char *) font;
 result->storage_as.string.dim = 0;
 no_interrupt(iflag);
 return(result);}

gdFontPtr get_gdFontPtr(LISP ptr)
{gdFontPtr fn;
 if (NTYPEP(ptr,tc_gdfont))
   err("not a gdFont",ptr);
 if (!(fn = (gdFontPtr) ptr->storage_as.string.data))
   err("gd Font deallocated",ptr);
 return(fn);}

LISP lgdPoint(LISP args)
{LISP result,l;
 long iflag,j,m,n = nlength(args);
 gdPointPtr pt;
 if ((n % 2) || (!n))
   err("must be an even positive length",args);
 m = n / 2;
 result = cons(NIL,NIL);
 result->type = tc_gdpoint;
 iflag = no_interrupt(1);
 pt =  (gdPointPtr) must_malloc(sizeof(gdPoint) * m);
 result->storage_as.string.data = (char *) pt;
 result->storage_as.string.dim = m;
 no_interrupt(iflag);
 for(j=0,l=args;j<m;++j,l=cddr(l))
   {pt[j].x = get_c_long(car(l));
    pt[j].y = get_c_long(cadr(l));}
 return(result);}

gdPointPtr get_gdPointPtr(LISP ptr,long *n)
{gdPointPtr pt;
 if (NTYPEP(ptr,tc_gdpoint))
   err("not a gdPoint",ptr);
 if (!(pt = (gdPointPtr) ptr->storage_as.string.data))
   err("gd point deallocated",ptr);
 *n = ptr->storage_as.string.dim;
 return(pt);}

LISP lgdPointx(LISP ptr,LISP j,LISP value)
{long n,i;
 gdPointPtr pt;
 pt = get_gdPointPtr(ptr,&n);
 i = get_c_long(j);
 if ((i < 0) || (i >= n)) err("index out of range",j);
 if NNULLP(value)
   pt[i].x = get_c_long(value);
 else
   value = flocons(pt[i].x);
 return(value);}


LISP lgdPointy(LISP ptr,LISP j,LISP value)
{long n,i;
 gdPointPtr pt;
 pt = get_gdPointPtr(ptr,&n);
 i = get_c_long(j);
 if ((i < 0) || (i >= n)) err("index out of range",j);
 if NNULLP(value)
   pt[i].y = get_c_long(value);
 else
   value = flocons(pt[i].y);
 return(value);}

LISP lgdImageGif(LISP im,LISP f)
{long iflag;
 iflag = no_interrupt(1);
 gdImageGif(get_gdImagePtr(im),get_c_file(f,stdout));
 no_interrupt(iflag);
 return(NIL);}

LISP lgdImageGifmem(LISP im,LISP b)
{long iflag,dim;
 size_t len;
 char *buffer;
 buffer = get_c_string_dim(b,&dim);
 len = dim;
 iflag = no_interrupt(1);
 gdImageGifmem(get_gdImagePtr(im),buffer,&len);
 no_interrupt(iflag);
 return(flocons(len));}

LISP lgdImageColorAllocate(LISP l)
{long iflag;
 int result;
 gdImagePtr im;
 int r,g,b;
 iflag = no_interrupt(1);
 im = get_gdImagePtr(poparg(&l,NIL));
 r = get_c_long(poparg(&l,NIL));
 g = get_c_long(poparg(&l,NIL));
 b = get_c_long(poparg(&l,NIL));
 result = gdImageColorAllocate(im,r,g,b);
 no_interrupt(iflag);
 return(flocons(result));}

LISP lgdImageColorClosest(LISP l)
{long iflag;
 int result;
 gdImagePtr im;
 int r,g,b;
 iflag = no_interrupt(1);
 im = get_gdImagePtr(poparg(&l,NIL));
 r = get_c_long(poparg(&l,NIL));
 g = get_c_long(poparg(&l,NIL));
 b = get_c_long(poparg(&l,NIL));
 result = gdImageColorClosest(im,r,g,b);
 no_interrupt(iflag);
 return(flocons(result));}

LISP lgdImageColorExact(LISP l)
{long iflag;
 int result;
 gdImagePtr im;
 int r,g,b;
 iflag = no_interrupt(1);
 im = get_gdImagePtr(poparg(&l,NIL));
 r = get_c_long(poparg(&l,NIL));
 g = get_c_long(poparg(&l,NIL));
 b = get_c_long(poparg(&l,NIL));
 result = gdImageColorExact(im,r,g,b);
 no_interrupt(iflag);
 return(flocons(result));}

LISP lgdImageLine(LISP l)
{gdImagePtr im;
 int x1,y1,x2,y2,color;
 im = get_gdImagePtr(poparg(&l,NIL));
 x1 = get_c_long(poparg(&l,NIL));
 y1 = get_c_long(poparg(&l,NIL));
 x2 = get_c_long(poparg(&l,NIL));
 y2 = get_c_long(poparg(&l,NIL));
 color = get_c_long(poparg(&l,NIL));
 gdImageLine(im,x1,y1,x2,y2,color);
 return(NIL);}

LISP lgdImageSetPixel(LISP l)
{gdImagePtr im;
 int x,y,color;
 im = get_gdImagePtr(poparg(&l,NIL));
 x = get_c_long(poparg(&l,NIL));
 y = get_c_long(poparg(&l,NIL));
 color = get_c_long(poparg(&l,NIL));
 gdImageSetPixel(im,x,y,color);
 return(NIL);}

LISP lgdImagePolygon(LISP im,LISP points,LISP color)
{gdPointPtr pt;
 long n;
 pt = get_gdPointPtr(points,&n);
 gdImagePolygon(get_gdImagePtr(im),
		pt,
		n,
		get_c_long(color));}

LISP lgdImageFilledPolygon(LISP im,LISP points,LISP color)
{gdPointPtr pt;
 long n;
 pt = get_gdPointPtr(points,&n);
 gdImageFilledPolygon(get_gdImagePtr(im),
		pt,
		n,
		get_c_long(color));}

LISP lgdImageRectangle(LISP l)
{gdImagePtr im;
 int x1,y1,x2,y2,color;
 im = get_gdImagePtr(poparg(&l,NIL));
 x1 = get_c_long(poparg(&l,NIL));
 y1 = get_c_long(poparg(&l,NIL));
 x2 = get_c_long(poparg(&l,NIL));
 y2 = get_c_long(poparg(&l,NIL));
 color = get_c_long(poparg(&l,NIL));
 gdImageRectangle(im,x1,y1,x2,y2,color);
 return(NIL);}

LISP lgdImageFilledRectangle(LISP l)
{gdImagePtr im;
 int x1,y1,x2,y2,color;
 im = get_gdImagePtr(poparg(&l,NIL));
 x1 = get_c_long(poparg(&l,NIL));
 y1 = get_c_long(poparg(&l,NIL));
 x2 = get_c_long(poparg(&l,NIL));
 y2 = get_c_long(poparg(&l,NIL));
 color = get_c_long(poparg(&l,NIL));
 gdImageFilledRectangle(im,x1,y1,x2,y2,color);
 return(NIL);}

LISP lgdImageArc(LISP l)
{gdImagePtr im;
 int cx,cy,w,h,s,e,color;
 im = get_gdImagePtr(poparg(&l,NIL));
 cx = get_c_long(poparg(&l,NIL));
 cy = get_c_long(poparg(&l,NIL));
 w = get_c_long(poparg(&l,NIL));
 h = get_c_long(poparg(&l,NIL));
 s = get_c_long(poparg(&l,NIL));
 e = get_c_long(poparg(&l,NIL));
 color = get_c_long(poparg(&l,NIL));
 gdImageArc(im,cx,cy,w,h,s,e,color);
 return(NIL);}


LISP lgdImageFillToBorder(LISP l)
{gdImagePtr im;
 int x,y,border,color;
 im = get_gdImagePtr(poparg(&l,NIL));
 x = get_c_long(poparg(&l,NIL));
 y = get_c_long(poparg(&l,NIL));
 border = get_c_long(poparg(&l,NIL));
 color = get_c_long(poparg(&l,NIL));
 gdImageFillToBorder(im,x,y,border,color);
 return(NIL);}

LISP lgdImageFill(LISP l)
{gdImagePtr im;
 int x,y,color;
 im = get_gdImagePtr(poparg(&l,NIL));
 x = get_c_long(poparg(&l,NIL));
 y = get_c_long(poparg(&l,NIL));
 color = get_c_long(poparg(&l,NIL));
 gdImageFill(im,x,y,color);
 return(NIL);}

/*

void gdImageSetBrush(gdImagePtr im, gdImagePtr brush)
void gdImageSetTile(gdImagePtr im, gdImagePtr tile)
void gdImageSetStyle(gdImagePtr im, int *style, int styleLength)

*/

LISP lgdImageChar(LISP l)
{gdImagePtr im;
 gdFontPtr font;
 int x,y,c,color;
 im = get_gdImagePtr(poparg(&l,NIL));
 font = get_gdFontPtr(poparg(&l,NIL));
 x = get_c_long(poparg(&l,NIL));
 y = get_c_long(poparg(&l,NIL));
 c = get_c_long(poparg(&l,NIL));
 color = get_c_long(poparg(&l,NIL));
 gdImageChar(im,font,x,y,c,color);
 return(NIL);}

LISP lgdImageCharUp(LISP l)
{gdImagePtr im;
 gdFontPtr font;
 int x,y,c,color;
 im = get_gdImagePtr(poparg(&l,NIL));
 font = get_gdFontPtr(poparg(&l,NIL));
 x = get_c_long(poparg(&l,NIL));
 y = get_c_long(poparg(&l,NIL));
 c = get_c_long(poparg(&l,NIL));
 color = get_c_long(poparg(&l,NIL));
 gdImageCharUp(im,font,x,y,c,color);
 return(NIL);}

LISP lgdImageString(LISP l)
{gdImagePtr im;
 gdFontPtr font;
 int x,y,color;
 char *c;
 im = get_gdImagePtr(poparg(&l,NIL));
 font = get_gdFontPtr(poparg(&l,NIL));
 x = get_c_long(poparg(&l,NIL));
 y = get_c_long(poparg(&l,NIL));
 c = get_c_string(poparg(&l,NIL));
 color = get_c_long(poparg(&l,NIL));
 gdImageString(im,font,x,y,c,color);
 return(NIL);}

LISP lgdImageStringUp(LISP l)
{gdImagePtr im;
 gdFontPtr font;
 int x,y,color;
 char *c;
 im = get_gdImagePtr(poparg(&l,NIL));
 font = get_gdFontPtr(poparg(&l,NIL));
 x = get_c_long(poparg(&l,NIL));
 y = get_c_long(poparg(&l,NIL));
 c = get_c_string(poparg(&l,NIL));
 color = get_c_long(poparg(&l,NIL));
 gdImageStringUp(im,font,x,y,c,color);
 return(NIL);}

LISP lgdImageColorTransparent(LISP im,LISP color)
{gdImageColorTransparent(get_gdImagePtr(im),get_c_long(color));
 return(NIL);}

LISP lgdImageInterlace(LISP im,LISP interlace)
{gdImageInterlace(get_gdImagePtr(im),get_c_long(interlace));
 return(NIL);}

void gdimage_prin1(LISP ptr,struct gen_printio *f)
{char buff[256];
 gdImagePtr im;
 if (im = (gdImagePtr) ptr->storage_as.string.data)
   sprintf(buff,"#<GDIMAGE %p %d by %d>",
	   im,im->sx,im->sy);
 else
   strcpy(buff,"#<GDIMAGE NULL>");
 gput_st(f,buff);}

void gdimage_gc_free(LISP ptr)
{gdImagePtr im;
 if (im = (gdImagePtr) ptr->storage_as.string.data)
   {gdImageDestroy(im);
    ptr->storage_as.string.data = NULL;}}

void gdfont_prin1(LISP ptr,struct gen_printio *f)
{char buff[256];
 gdFontPtr fnt;
 if (fnt = (gdFontPtr) ptr->storage_as.string.data)
   sprintf(buff,"#<GDFONT %p %d by %d>",
	   fnt,fnt->w,fnt->h);
 else
   strcpy(buff,"#<GDFONT NULL>");
 gput_st(f,buff);}

void gdfont_gc_free(LISP ptr)
{gdFontPtr fnt;
 if ((fnt = (gdFontPtr) ptr->storage_as.string.data) &&
     ptr->storage_as.string.dim)
   /* there is no api in gd 1.2 for loading a file from a file.
      the included fonts are all static data declarations. */
   ;
 }

void gdpoint_prin1(LISP ptr,struct gen_printio *f)
{char buff[256];
 gdPointPtr pt;
 pt = (gdPointPtr) ptr->storage_as.string.data;
 sprintf(buff,"#<GDPOINT %p %d>",
	 pt,ptr->storage_as.string.dim);
 gput_st(f,buff);}

void gdpoint_gc_free(LISP ptr)
{gdPointPtr pt;
 if (pt = (gdPointPtr) ptr->storage_as.string.data)
   {free(pt);
    ptr->storage_as.string.data = NULL;
    ptr->storage_as.string.dim = 0;}}

LISP gdfont_w(LISP ptr)
{gdFontPtr font;
 font = get_gdFontPtr(ptr);
 return(flocons(font->w));}

LISP gdfont_h(LISP ptr)
{gdFontPtr font;
 font = get_gdFontPtr(ptr);
 return(flocons(font->h));}

void init_gd(void)
{long j;
 tc_gdimage = allocate_user_tc();
 set_gc_hooks(tc_gdimage,
	      NULL,
	      NULL,
	      NULL,
	      gdimage_gc_free,
	      &j);
 set_print_hooks(tc_gdimage,gdimage_prin1);
 tc_gdfont = allocate_user_tc();
 set_gc_hooks(tc_gdfont,
	      NULL,
	      NULL,
	      NULL,
	      gdfont_gc_free,
	      &j);
 set_print_hooks(tc_gdfont,gdfont_prin1);
 tc_gdpoint = allocate_user_tc();
 set_gc_hooks(tc_gdpoint,
	      NULL,
	      NULL,
	      NULL,
	      gdpoint_gc_free,
	      &j);
 set_print_hooks(tc_gdpoint,gdpoint_prin1);
 setvar(cintern("gdFontGiant"),
	lcgdFontCreate(gdFontGiant),
	NIL);
 setvar(cintern("gdFontLarge"),
	lcgdFontCreate(gdFontLarge),
	NIL);
 setvar(cintern("gdFontMediumBold"),
	lcgdFontCreate(gdFontMediumBold),
	NIL);
 setvar(cintern("gdFontSmall"),
	lcgdFontCreate(gdFontSmall),
	NIL);
 setvar(cintern("gdFontTiny"),
	lcgdFontCreate(gdFontTiny),
	NIL);
 setvar(cintern("gdStyled"),flocons(gdStyled),NIL);
 setvar(cintern("gdBrushed"),flocons(gdBrushed),NIL);
 setvar(cintern("gdStyledBrushed"),flocons(gdStyledBrushed),NIL);
 setvar(cintern("gdTiled"),flocons(gdTiled),NIL);
 setvar(cintern("gdTransparent"),flocons(gdTransparent),NIL);
 init_subr_2("gdImageCreate",lgdImageCreate);
 init_subr_1("gdImageCreateFromGif",lgdImageCreateFromGif);
 init_subr_1("gdImageCreateFromXbm",lgdImageCreateFromXbm);
 init_subr_2("gdImageGif",lgdImageGif);
 init_subr_2("gdImageGifmem",lgdImageGifmem);
 init_lsubr("gdImageColorAllocate",lgdImageColorAllocate);
 init_lsubr("gdImageColorClosest",lgdImageColorClosest);
 init_lsubr("gdImageColorExact",lgdImageColorExact);
 init_lsubr("gdImageLine",lgdImageLine);
 init_lsubr("gdImageRectangle",lgdImageRectangle);
 init_lsubr("gdImageFilledRectangle",lgdImageFilledRectangle);
 init_lsubr("gdPoint",lgdPoint);
 init_subr_3("gdPoint.x",lgdPointx);
 init_subr_3("gdPoint.y",lgdPointy);
 init_lsubr("gdImageSetPixel",lgdImageSetPixel);
 init_subr_3("gdImagePolygon",lgdImagePolygon);
 init_subr_3("gdImageFilledPolygon",lgdImageFilledPolygon);
 init_lsubr("gdImageArc",lgdImageArc);
 init_lsubr("gdImageChar",lgdImageChar);
 init_lsubr("gdImageCharUp",lgdImageCharUp);
 init_lsubr("gdImageString",lgdImageString);
 init_lsubr("gdImageStringUp",lgdImageStringUp);
 init_subr_2("gdImageColorTransparent",lgdImageColorTransparent);
 init_subr_2("gdImageInterlace",lgdImageInterlace);
 init_lsubr("gdImageFillToBorder",lgdImageFillToBorder);
 init_lsubr("gdImageFill",lgdImageFill);
 init_subr_1("gdFont.w",gdfont_w);
 init_subr_1("gdFont.h",gdfont_h);
 init_gd_version();}
