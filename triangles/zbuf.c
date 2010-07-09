/*
 * C code to implement a Z-buffer
 */

#include <stdio.h>
#include "Python.h"

#define max(a,b) ((a) >= (b) ? (a) : (b))
#define min(a,b) ((a) <= (b) ? (a) : (b))

#define HORIZON  1.0e10

int height, width;
struct pixel
{
  double z;
  double r, g, b;
};
struct pixel *band, *bandptr;

struct big_pixel
{
  double x, y, z;
  double r, g, b;
};

static PyObject *
zbuf_init(PyObject *self, PyObject *args)
{
  if (band != NULL)
    goto done;
  if (!PyArg_ParseTuple(args, "ii", &width, &height))
    return NULL;
  band = malloc(width * height * sizeof(struct pixel));
  if (band == NULL)
    {
      PyErr_NoMemory();
      return NULL;
    }
 done:
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
zbuf_deinit(PyObject *self, PyObject *args)
{
  if (band == NULL)
    goto done;
  if (!PyArg_ParseTuple(args, ""))
    return NULL;
  free(band);
  band = NULL;
 done:
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
zbuf_clear(PyObject *self, PyObject *args)
{
  int i, rbackground, gbackground, bbackground;
  rbackground = gbackground = bbackground = 0;
  if (!PyArg_ParseTuple(args, "|iii", &rbackground, &gbackground, &bbackground))
    return NULL;
  for (i = 0; i < width * height; i++)
    {
      band[i].r = rbackground;
      band[i].g = gbackground;
      band[i].b = bbackground;
      band[i].z = HORIZON;
    }
  Py_INCREF(Py_None);
  return Py_None;
}

static void half_triangle(int ytop,
                          struct big_pixel *pstart,
                          struct big_pixel *pfinish,
                          struct big_pixel *pbeyond)
{

  int x, y, ybottom;
  struct big_pixel *pleft, *pright;
  double mult;
  double xleft, xright, dxleft, dxright;
  double z, dz, zleft, zright, dzleft, dzright;
  double dr, rleft, rright, drleft, drright;
  double dg, gleft, gright, dgleft, dgright;
  double db, bleft, bright, dbleft, dbright;

  ybottom = ytop + height;

  if (! ((ytop >= pstart->y && ytop <= pfinish->y) ||
	 (ytop <= pstart->y && ytop >= pfinish->y) ||
	 (ybottom >= pstart->y && ybottom <= pfinish->y) ||
	 (ybottom <= pstart->y && ybottom >= pfinish->y) ||
	 (pstart->y >= ytop && pstart->y <= ybottom) ||
	 (pfinish->y >= ytop && pfinish->y <= ybottom)) )
    return;

  /* Figure out whether finish or beyond represents the left side */
  if ((pfinish->y < pstart->y) ^
      ((pbeyond->y - pstart->y) * (pfinish->x - pstart->x) <
       (pfinish->y - pstart->y) * (pbeyond->x - pstart->x)))
    {
      pleft = pfinish;
      pright = pbeyond;
    }
  else
    {
      pleft = pbeyond;
      pright = pfinish;
    }

  mult = 1.0 / (pleft->y - pstart->y);
  dxleft = mult * (pleft->x - pstart->x);
  dzleft = mult * (pleft->z - pstart->z);
  drleft = mult * (pleft->r - pstart->r);
  dgleft = mult * (pleft->g - pstart->g);
  dbleft = mult * (pleft->b - pstart->b);
  mult = 1.0 / (pright->y - pstart->y);
  dxright = mult * (pright->x - pstart->x);
  dzright = mult * (pright->z - pstart->z);
  drright = mult * (pright->r - pstart->r);
  dgright = mult * (pright->g - pstart->g);
  dbright = mult * (pright->b - pstart->b);

  for (bandptr = band, y = ytop; y < ybottom; y++, bandptr += width)
    {
      if ((y >= pstart->y && y <= pfinish->y) ||
	  (y <= pstart->y && y >= pfinish->y))
	{
	  double dy = y - pstart->y;
	  xleft = pstart->x + dxleft * dy;
	  xright = pstart->x + dxright * dy;
	  if (xleft < xright)
	    {
	      zleft = pstart->z + dzleft * dy;
	      rleft = pstart->r + drleft * dy;
	      gleft = pstart->g + dgleft * dy;
	      bleft = pstart->b + dbleft * dy;
	      zright = pstart->z + dzright * dy;
	      rright = pstart->r + drright * dy;
	      gright = pstart->g + dgright * dy;
	      bright = pstart->b + dbright * dy;
	      dz = (zright - zleft) / (xright - xleft);
	      dr = (rright - rleft) / (xright - xleft);
	      dg = (gright - gleft) / (xright - xleft);
	      db = (bright - bleft) / (xright - xleft);
	      for (x = 0; x < width; x++)
		{
		  double dx = x - xleft;
		  z = zleft + dz * dx;
		  if (x >= xleft && x <= xright &&
		      z < bandptr[x].z)
		    {
		      bandptr[x].z = z;
		      bandptr[x].r = rleft + dr * dx;
		      bandptr[x].g = gleft + dg * dx;
		      bandptr[x].b = bleft + db * dx;
		    }
		}
	    }
	}
    }
}

#define SWAP(p1,p2) \
  { struct big_pixel ptmp; ptmp = p1; p1 = p2; p2 = ptmp; }

static PyObject *
zbuf_tri(PyObject *self, PyObject *args)
{
  int ytop;
  struct big_pixel p0, p1, p2;

  if (!PyArg_ParseTuple(args, "idddddddddddddddddd",
			&ytop,
			&p0.x, &p0.y, &p0.z, &p0.r, &p0.g, &p0.b,
			&p1.x, &p1.y, &p1.z, &p1.r, &p1.g, &p1.b,
			&p2.x, &p2.y, &p2.z, &p2.r, &p2.g, &p2.b))
    return NULL;
  if (((p0.y < p2.y) && (p2.y < p1.y)) || ((p1.y < p2.y) && (p2.y < p0.y)))
    SWAP(p1, p2);
  if (((p2.y < p0.y) && (p0.y < p1.y)) || ((p1.y < p0.y) && (p0.y < p2.y)))
    SWAP(p0, p1);
  half_triangle(ytop, &p0, &p1, &p2);
  half_triangle(ytop, &p2, &p1, &p0);
  Py_INCREF(Py_None);
  return Py_None;
}

static PyObject *
zbuf_tostring(PyObject *self, PyObject *args)
{
  char *str;
  int i;
  if (!PyArg_ParseTuple(args, ""))
    return NULL;
  str = malloc(width * height * 3);
  for (i = 0; i < width * height; i++)
    {
      str[3*i + 0] = (unsigned char)band[i].r;
      str[3*i + 1] = (unsigned char)band[i].g;
      str[3*i + 2] = (unsigned char)band[i].b;
    }
  return PyString_FromStringAndSize(str, width * height * 3);
}

/* List of functions defined in the module */

static PyMethodDef zbuf_methods[] = {
  {"init",        zbuf_init,        1},
  {"deinit",      zbuf_deinit,      1},
  {"clear",       zbuf_clear,       1},
  {"tri",         zbuf_tri,         1},
  {"tostring",    zbuf_tostring,    1},
  {NULL,          NULL}           /* sentinel */
};


/* Initialization function for the module (*must* be called initzbuf) */

DL_EXPORT(void)
initzbuf()
{
  Py_InitModule("zbuf", zbuf_methods);
}
