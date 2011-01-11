/*  Scheme Session Management, part of guile-web
    Copyright (C) 2002,2003 Clinton Ebadi

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

/* This implements session handling in Scheme using libcgi */

#include <stdio.h> /* Needed by cgi.h, it doesn't include it by itself */
#include <libcgi/cgi.h>
#include <libcgi/session.h>
#include <libguile.h>
#include <stdlib.h>
#include <stdio.h>

SCM scgi_session_destroy ()
{
  int retval = cgi_session_destroy ();

  fflush (stdout);

  if (retval)
    return SCM_BOOL_T;
  return SCM_BOOL_F;
}

SCM scgi_session_register_var (const SCM name, const SCM value)
{
  /* todo: remove gh_scm2newstr when Guile 1.8 is released */
  char *n;
  char *v = gh_scm2newstr (value, NULL);
  int retval;

  if (SCM_SYMBOLP(name))
    {
      n = gh_symbol2newstr (name, NULL);
    }
  else
    {
      n = gh_scm2newstr (name, NULL);
    }

  retval = cgi_session_register_var (n, v);
  
  free (n);
  free (v);

  if (retval)
    {
      return SCM_BOOL_T;
    }
  else
    {
      return SCM_BOOL_F;
    }
}

SCM scgi_session_alter_var (const SCM name, const  SCM new_value)
{
  /* todo: remove gh_scm2newstr when Guile 1.8 is released */
  char *n;
  char *v = gh_scm2newstr (new_value, NULL);
  int retval;

  if (SCM_SYMBOLP(name))
    {
      n = gh_symbol2newstr (name, NULL);
    }
  else
    {
      n = gh_scm2newstr (name, NULL);
    }

  retval = cgi_session_alter_var (n, v);

  free (n);
  free (v);
  if (retval)
    {
      return SCM_BOOL_T;
    }
  else
    {
      return SCM_BOOL_F;
    }
}


SCM scgi_session_var_exists (const SCM name)
{
  /* todo: remove gh_scm2newstr when Guile 1.8 is released */
  char *n;
  int retval;

  if (SCM_SYMBOLP(name))
    {
      n = gh_symbol2newstr (name, NULL);
    }
  else
    {
      n = gh_scm2newstr (name, NULL);
    }

  retval = cgi_session_var_exists (n);

  free (n);
  
  if (retval)
    {
      return SCM_BOOL_T;
    }
  else
    {
      return SCM_BOOL_F;
    }
}

SCM scgi_session_unregister_var (const SCM name)
{
  /* todo: remove gh_scm2newstr when Guile 1.8 is released */
  char *n;
  int retval;

  if (SCM_SYMBOLP(name))
    {
      n = gh_symbol2newstr (name, NULL);
    }
  else
    {
      n = gh_scm2newstr (name, NULL);
    }

  retval = cgi_session_unregister_var (n);

  free (n);

  if (retval)
    {
      return SCM_BOOL_T;
    }
  else
    {
      return SCM_BOOL_F;
    }
}

SCM scgi_session_start()
{
  cgi_init (); // init libcgi

  if (cgi_session_start ())
    {
      fflush (stdout);
      return SCM_BOOL_T;
    }
  else
    {
      fflush (stdout);

      return SCM_BOOL_F;
    }
}

SCM scgi_session_cookie_name (SCM name)
{
  /* todo: remove gh_scm2newstr when Guile 1.8 is released */
  char *n;

  if (SCM_SYMBOLP(name))
    {
      n = gh_symbol2newstr (name, NULL);
    }
  else
    {
      n = gh_scm2newstr (name, NULL);
    }

  cgi_session_cookie_name (n);

  free (n);

  return SCM_UNSPECIFIED;
}

SCM scgi_session_save_path (SCM path)
{
  /* todo: remove gh_scm2newstr when Guile 1.8 is released */
  char *p;

  if (SCM_SYMBOLP(path))
    {
      p = gh_symbol2newstr (path, NULL);
    }
  else
    {
      p = gh_scm2newstr (path, NULL);
    }

  cgi_session_save_path (p);

  free (p);

  return SCM_UNSPECIFIED;
}

SCM scgi_session_var (SCM name)
{
  /* todo: remove gh_scm2newstr when Guile 1.8 is released */
  char *n;
  char *retval;

  if (SCM_SYMBOLP(name))
    {
      n = gh_symbol2newstr (name, NULL);
    }
  else
    {
      n = gh_scm2newstr (name, NULL);
    }

  retval = cgi_session_var (n);
  
  free (n);
  
  return scm_makfrom0str (retval);
}

void scgi_session_init ()
{
  scm_c_define_gsubr ("session:_start", 0, 0, 0,
		      scgi_session_start);
  scm_c_define_gsubr ("session:destroy", 0, 0, 0,
		      scgi_session_destroy);
  scm_c_define_gsubr ("session:_register-var", 2, 0, 0,
		      scgi_session_register_var);
  scm_c_define_gsubr ("session:_alter-var", 2, 0, 0,
		      scgi_session_alter_var);
  scm_c_define_gsubr ("session:_var-exists?", 1, 0, 0,
		      scgi_session_var_exists);
  scm_c_define_gsubr ("session:_unregister-var", 1, 0, 0,
		      scgi_session_unregister_var);
  scm_c_define_gsubr ("session:set-cookie-name!", 1, 0, 0,
		      scgi_session_cookie_name);
  scm_c_define_gsubr ("session:_get-value", 1, 0, 0,
		      scgi_session_var);
  scm_c_define_gsubr ("session:set-save-path!", 1, 0, 0,
		      scgi_session_save_path);
}
