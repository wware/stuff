;;; (www url-coding) --- URL character coding: decode/encode

;; Copyright (C) 2009 Thien-Thi Nguyen
;; Copyright (C) 2004, 2005, 2007 Free Software Foundation, Inc.
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

;; The (www url-coding) module is fully documented in the guile-www.info file.

;;; Code:

(define-module (www url-coding)
  #:export (url-coding:decode
            url-coding:encode)
  #:use-module (ice-9 regex))

;; Return a new string made from url-decoding @var{str}.  Specifically,
;; turn @code{+} into space, and hex-encoded @code{%XX} strings into
;; their eight-bit characters.
;;
(define (url-coding:decode str)
  ;; Implementation Questions: Is a regexp faster than character scanning?
  ;; Does it incur more overhead (which may be more important for code that
  ;; frequently gets restarted)?
  (regexp-substitute/global
   #f "\\+|%([0-9A-Fa-f][0-9A-Fa-f])" str
   'pre
   (lambda (m)
     (cond ((string=? "+" (match:substring m 0)) " ")
           (else (integer->char
                  (string->number
                   (match:substring m 1)
                   16)))))
   'post))

;; Return a new string made from url-encoding @var{str},
;; unconditionally transforming those in @var{reserved-chars}, a list
;; of characters, in addition to those in the standard (internal) set.
;;
(define (url-coding:encode str reserved-chars)
  ;; Can't be done easily with a regexp: we would have to construct a
  ;; regular expression like "[\277-\377]", for example, and Guile
  ;; strings don't let you interpolate character literals.  Pity.
  (with-output-to-string
    (lambda ()
      (for-each (lambda (ch)
                  (if (and (safe-char? ch)
                           (not (memv ch reserved-chars)))
                      (display ch)
                      (begin
                        (display #\%)
                        (display (number->string (char->integer ch) 16)))))
                (string->list str)))))

(define safe-chars (append! (string->list "$-_.+!*'(),")
                            ;; reserved
                            (string->list ";/?:@&=")))

(define (safe-char? ch)
  ;; “Thus, only alphanumerics, the special characters "$-_.+!*'(),", and
  ;; reserved characters used for their reserved purposes may be used
  ;; unencoded within a URL.” RFC 1738, #2.2.
  (or (char-alphabetic? ch)
      (char-numeric? ch)
      (memv ch safe-chars)))

;;; (www url-coding) ends here
