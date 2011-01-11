;;; (www utcsec) --- Seconds after epoch, UTC

;; Copyright (C) 2008 Thien-Thi Nguyen
;; Copyright (C) 2007 Free Software Foundation, Inc.
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

;;; Commentary:

;; The (www utcsec) module is fully documented in the guile-www.info file.

;;; Code:

(define-module (www utcsec)
  #:export (format-utcsec
            rfc1123-date<-
            <-rfc1123-date
            <-mtime
            <-ctime
            rfc1123-now))

;; Write to output port @var{port} the @var{utc-seconds}
;; formatted according to @var{format} (a string).
;; If @var{port} is @code{#f}, return the output string, instead.
;; This uses @code{strftime}, q.v.
;;
(define (format-utcsec port format utc-seconds)
  (simple-format port (strftime format (gmtime utc-seconds))))

(define *rfc1123-format* "%a, %d %b %Y %T GMT")

;; Write to output port @var{port} the @var{utc-seconds}
;; formatted according to RFC1123.  If @var{port} is @code{#f},
;; return the output string, instead.
;;
;; For example:
;;
;; @example
;; (rfc1123-date<- #f 1167791441)
;; @result{} "Wed, 03 Jan 2007 02:30:41 GMT"
;; @end example
;;
(define (rfc1123-date<- port utc-seconds)
  (format-utcsec port *rfc1123-format* utc-seconds))

;; Parse the RFC1123-compliant date string @var{s}, and
;; return the utc-seconds it represents.
;;
;; For example:
;;
;; @example
;; (<-rfc1123-date "Wed, 03 Jan 2007 02:30:41 GMT")
;; @result{} 1167791441
;; @end example
;;
(define (<-rfc1123-date s)
  (car (mktime (car (strptime *rfc1123-format* s)) "UTC")))

(define (<-*time which who filespec)
  (let ((si (cond ((vector? filespec) filespec)
                  ((or (port? filespec) (string? filespec)) (stat filespec))
                  (else (scm-error 'wrong-type-arg who
                                   "Bad filespec in position 1: ~S"
                                   (list filespec)
                                   #f)))))
    (car (mktime (gmtime (which si)) "UTC"))))

;; Return the utc-seconds of the modification time of @var{filespec}.
;; @var{filespec} can be a filename (string), a port opened on a
;; @code{stat}able file, or the object resulting from a @code{stat}
;; on one of these.
;;
;; For example:
;;
;; @example
;; (= (<-mtime "COPYING")
;;    (<-mtime (open-input-file "COPYING"))
;;    (<-mtime (stat "COPYING")))
;; @result{} #t
;; @end example
;;
(define (<-mtime filespec)
  (<-*time stat:mtime '<-mtime filespec))

;; Return the utc-seconds of the creation time of @var{filespec}.
;; @var{filespec} can be a filename (string), a port opened on a
;; @code{stat}able file, or the object resulting from a @code{stat}
;; on one of these.
;;
(define (<-ctime filespec)
  (<-*time stat:ctime '<-ctime filespec))

;; The "current time" formatted according to RFC1123.
;;
(define (rfc1123-now)
  (rfc1123-date<- #f (current-time)))

;;; (www utcsec) ends here
