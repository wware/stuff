;;; (www server-utils form-2-form) --- Unflatten POSTed form data

;; Copyright (C) 2009 Thien-Thi Nguyen
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

(define-module (www server-utils form-2-form)
  #:export (parse-form)
  #:use-module (www url-coding)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-14)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 rw)
  #:use-module (ice-9 and-let-star))

(define +boundary-rx+ (make-regexp "boundary=\"*(.[^\"\r\n]*)\"*"))
(define +name-rx+     (make-regexp "name=\"([^\"]*)\""))
(define +filename-rx+ (make-regexp "filename=\"*([^\"\r]*)\"*"))
(define +type-rx+     (make-regexp "Content-Type: ([^\r]*)\r"
                                   (logior regexp/icase)))

(define ((m1-extract string) rx)
  (and-let* ((m (regexp-exec rx string)))
    (match:substring m 1)))

;; Parse @var{raw-data} as raw form response data of enctype
;; @samp{multipart/form-data} and return an alist.
;;
;; @var{content-type-more} is a string that should include the
;; @code{boundary="@dots{}"} information.  (This parameter name reflects the
;; typical source of such a string, the Content-Type header value, after the
;; @samp{multipart/form-data}.)
;;
;; Each element of the alist has the form @code{(@var{name} . @var{value})},
;; where @var{name} is a string and @var{value} is either a string or four
;; values (extractable by @code{call-with-values}):
;;
;; @table @var
;; @item filename
;; A string, or @code{#f}.
;; @item type
;; A string representing the MIME type of the uploaded file.
;; @item raw-headers
;; A string, including all eol @sc{crlf} chars.
;; Incidentally, the @var{type} should be
;; (redundantly) visible in one of the headers.
;; @item squeeze
;; A procedure that takes one arg @var{abr} (standing for access byte
;; range).  If @var{abr} is @code{#f}, then internal references to the
;; uploaded file's data are dropped.  Otherwise, @var{abr} should be a
;; procedure that takes three arguments: a string, a beginning index
;; (integer, inclusive), and an ending index (integer, exclusive).
;; @end table
;;
;; If there is no type information, @var{value} is a simple non-empty string,
;; and no associated information (filename, raw-headers, squeeze) is kept.
;;
;; @code{parse-form} ignores @dfn{degenerate uploads}, that is those parts
;; of @code{raw-data} where the part header specifies no filename and the
;; part content-length is zero or unspecified.
;;
(define (parse-form content-type-more raw-data)

  (let ((v '()))

    (define (determine-boundary s)
      (string-append "--" ((m1-extract s) +boundary-rx+)))

    (define (v! name value)
      (set! v (acons name value v)))

    (define (u! name filename type bov eov raw-headers)
      ;; ignore degenerate (no filename and no length) upload
      (or (and (or (not filename)
                   (string-null? filename))
               (= bov eov))
          (v! name (values filename type raw-headers
                           (let ((raw raw-data))
                             ;; access byte range
                             (lambda (abr)
                               (if (and raw abr)
                                   (abr raw bov eov)
                                   (set! raw #f))))))))

    (let level ((bor 0) (eor (string-length raw-data))
                (boundary (determine-boundary content-type-more))
                (parent-name #f))

      (define (find-bound from)
        (string-contains raw-data boundary from))

      (let get-pair ((bop (find-bound bor)))
        (set! bop (+ bop (string-length boundary)))
        (and-let* (((<= (+ 2 bop) eor))
                   ((string-prefix? "\r\n" raw-data 0 2 bop))
                   (boh (+ 2 bop))
                   (bov (+ 4 (string-contains raw-data "\r\n\r\n" boh)))
                   (headers (substring/shared raw-data boh (- bov 2)))
                   (hx (m1-extract headers))
                   (name (or parent-name (hx +name-rx+)))
                   (eop (find-bound bov))
                   (eov (- eop 2)))
          (or (and-let* ((type (hx +type-rx+)))
                (if (and (not parent-name) ; only recurse once
                         (string-contains type "multipart/mixed"))
                    (level bov eov (determine-boundary type) name)
                    (u! name (hx +filename-rx+) type bov eov headers)))
              (= bov eov)
              (v! name (substring/shared raw-data bov eov)))
          (get-pair eop))))

    (reverse! v)))

;;; (www server-utils form-2-form) ends here
