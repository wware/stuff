;;; (www server-utils log) --- Love notes to other programs

;; Copyright (C) 2009 Thien-Thi Nguyen
;; Copyright (C) 2004, 2006, 2007 Free Software Foundation, Inc.
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

(define-module (www server-utils log)
  ;; naming convention: log-SOMETHING-proc
  #:export (log-http-response-proc))

;; Return a procedure that writes an HTTP response log entry to @var{port}.
;; The procedure is called with args @var{client}, @var{method}, @var{upath}
;; (strings or symbols) and @var{status} (either an atom or a list), and
;; writes a one-line entry of the form:
;;
;; @example
;; CLIENT - - [YYYY-MM-DD:HH:MM:SS TZ] "METHOD UPATH" STATUS1 STATUS2...
;; @end example
;;
;; where the @samp{YYYY..TZ} are the year, month, day, hour, minute,
;; second and timezone components, respectively, of the @code{localtime}
;; representation of the current time; and @samp{STATUSn} are the
;; space-separated elements of @var{status}.
;;
;; Optional second arg @var{gmtime?} non-@code{#f} means use
;; @code{gmtime} instead of @code{localtime}.  Optional third arg
;; @var{stamp-format} specifies a format string passed to
;; @code{strftime} to use for the timestamp portion that appears between
;; the square braces (default: "%Y-%m-%d:%H:%M:%S %Z").
;;
;; Optional fourth arg @var{method-pair?} non-@code{#f} means that
;; @var{method} is expected to be a pair @code{(@var{meth}
;; . @var{vers})}, in which case the portion between the double quotes
;; becomes "@var{meth} @var{upath} @var{vers}".  This is to support
;; excruciating conformity to Apache for the benefit of downstream
;; programs that might fall over less than gracefully otherwise.  Please
;; enjoy the slack.
;;
;; The buffering mode for @var{port} is set to line-buffered.
;;
;;-sig: (port [gmtime? [stamp-format [method-pair?]]])
;;
(define (log-http-response-proc port . opts)
  (setvbuf port _IOLBF)
  (let* ((len (length opts))
         (rep (or (and (<= 1 len) (car opts) gmtime) localtime))
         (tfmt (or (and (<= 2 len) (cadr opts)) "%Y-%m-%d:%H:%M:%S %Z"))
         (mpair? (and (<= 3 len) (caddr opts)))
         (meth (if mpair? car identity))
         (vers (if mpair?
                   (lambda (x) (simple-format #f " ~A" (cdr x)))
                   (lambda (x) ""))))
    ;; rv
    (lambda (client method upath status)
      (simple-format port "~A - - [~A] \"~A ~A~A\" ~A"
                     client
                     (strftime tfmt (rep (current-time)))
                     (meth method) upath (vers method)
                     (if (pair? status) (car status) status))
      (and (pair? status)
           (for-each (lambda (x)
                       (simple-format port " ~A" x))
                     (cdr status)))
      (newline port))))

;;; (www server-utils log) ends here
