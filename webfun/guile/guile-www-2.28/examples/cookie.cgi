#!/usr/local/bin/guile -s
!#
;;; cookie.cgi --- cgi script using cookies

;; Copyright (C) 2002 Free Software Foundation, Inc.
;;
;; This file is part of Guile-WWW.
;;
;; Guile-WWW is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; Guile-WWW is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with Guile-WWW; see the file COPYING.  If not,
;; write to the Free Software Foundation, Inc., 51 Franklin Street,
;; Fifth Floor, Boston, MA  02110-1301  USA

;;; Author: Aaron VanDevender <sig@netdot.net>


;; Initialize cgi
(use-modules (www cgi))
(cgi:init)

;; Get a list of values associated with formvar1.
;; This works just like before except now
;; multipart/formdata "just works."
(define formvar1list (cgi:values "formvar1"))

;; Get a single value associated with formvar2.
(define formvar2 (cgi:value "formvar2"))

;; Get an uploaded file
;; After cgi:upload or cgi:uplods is called, filedata
;; is the only reference to the actual data, so cgi:upload
;; can only be called once per uploadformvar.
(define remote-filename (cgi:value "uploadformvar"))
(define filedata2 (cgi:upload "uploadformvar"))
(define filedata2-list (cgi:uploads "uploadformvar2"))


;; Get a list of cookie values
(define cookie-value-list (cgi:cookies "cookiename"))

;; Get a cookie value
(define cookie-value (cgi:cookie "cookiename"))

;; Make a cookie string suitable for inclusion in an HTTP
;; response header.
(define cookie-string (cgi:make-cookie "cookiename"
				       "cookievalue"
				       #:path "/urlpath"
				       #:domain "mydomain.org"
				       #:expires "Mon 01-Apr-2002 12:34:56 GMT"
				       #:secure #t))

;;; cookie.cgi ends here
