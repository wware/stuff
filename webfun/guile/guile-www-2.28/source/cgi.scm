;;; (www cgi) --- Common Gateway Interface support

;; Copyright (C) 2007, 2008, 2009 Thien-Thi Nguyen
;; Copyright (C) 1997, 2001, 2002, 2003, 2004, 2005,
;;   2006 Free Software Foundation, Inc.
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

;; The (www cgi) module is fully documented in the guile-www.info file.

;;; Code:

(define-module (www cgi)
  #:export (cgi:init
            cgi:getenv
            cgi:nv-pairs
            cgi:values cgi:value
            cgi:names
            cgi:form-data?
            cgi:uploads cgi:upload
            cgi:cookie-names
            cgi:cookies cgi:cookie)
  #:autoload (www server-utils parse-request) (alist<-query read-body)
  #:autoload (www server-utils cookies) (simple-parse-cookies)
  #:autoload (www server-utils form-2-form) (parse-form)
  #:use-module (ice-9 and-let-star)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-14))

(define (collate alist)
  (let ((rv '()))
    (for-each (lambda (k v)
                (set! rv (or (and-let* ((old (assoc-ref rv k)))
                               (append! old (list v))
                               rv)
                             (acons k (list v) rv))))
              (map car alist)
              (map cdr alist))
    (reverse! rv)))

(define ws/comma-split
  (let ((cs (char-set-complement (char-set-adjoin char-set:whitespace #\,))))
    (lambda (string)
      (string-tokenize string cs))))

;;; CGI environment variables.

(define (env-extraction-methods)        ; => (VAR METHOD ...)
  (define (server-sw-info)
    (and-let* ((sw (getenv "SERVER_SOFTWARE")))
      (list sw (string-index sw #\/))))
  (define (server-pr-info)
    (and-let* ((pr (getenv "SERVER_PROTOCOL")))
      (list pr (string-index pr #\/))))
  (define (extract make-args proc)
    (apply-to-args (make-args) proc))
  ;; rv -- methods may be a string to be passed to ‘getenv’, or a thunk
  `(server-hostname
    "SERVER_NAME"
    gateway-interface
    "GATEWAY_INTERFACE"
    server-port
    ,(lambda () (and=> (getenv "SERVER_PORT") string->number))
    request-method
    "REQUEST_METHOD"
    path-info
    "PATH_INFO"
    path-translated
    "PATH_TRANSLATED"
    script-name
    "SCRIPT_NAME"
    query-string
    "QUERY_STRING"
    remote-host
    "REMOTE_HOST"
    remote-addr
    "REMOTE_ADDR"
    authentication-type
    "AUTH_TYPE"
    remote-user
    "REMOTE_USER"
    remote-ident
    "REMOTE_IDENT"
    content-type
    "CONTENT_TYPE"
    content-length
    ,(lambda () (or (and=> (getenv "CONTENT_LENGTH")
                           string->number)
                    0))
    http-user-agent
    "HTTP_USER_AGENT"
    http-cookie
    "HTTP_COOKIE"
    server-software-type
    ,(lambda () (extract server-sw-info
                         (lambda (sw slash)
                           (if slash
                               (substring/shared sw 0 slash)
                               sw))))
    server-software-version
    ,(lambda () (extract server-sw-info
                         (lambda (sw slash)
                           (and slash (substring/shared sw (1+ slash))))))
    server-protocol-name
    ,(lambda () (extract server-pr-info
                         (lambda (pr slash)
                           (substring/shared pr 0 slash))))
    server-protocol-version
    ,(lambda () (extract server-pr-info
                         (lambda (pr slash)
                           (substring/shared pr (1+ slash)))))
    http-accept-types
    ,(lambda () (or (and=> (getenv "HTTP_ACCEPT") ws/comma-split)
                    ;; SHOULD be set (RFC3875, 4.1.18) but sometimes isn't
                    '()))))

(define *env-extraction*
  (let ((ht (make-hash-table 23)))
    (let loop ((ls (env-extraction-methods)))
      (or (null? ls)
          (let ((var (car ls)))
            (hashq-set! ht var
                        (let ((v (cadr ls)))
                          (if (string? v)
                              (lambda () (getenv v))
                              v)))
            (loop (cddr ls)))))
    ht))

(define (env-look key)                  ; may return #f
  ((hashq-ref *env-extraction* key
              (lambda ()
                (error "unrecognized key:" key)))))

;;; CGI context closure.

(define (make-ccc)
  (let ((P '())                         ; form variables as pairs
        (V '())                         ; form variables collated
        (U '()) (pre-squeezed? #t)      ; file uploads
        (C '()))                        ; cookies

    (define (init! opts)
      (set! P '()) (set! V '()) (set! U '())
      (set! pre-squeezed? (not (memq 'uploads-lazy opts)))
      (and-let* ((len (env-look 'content-length))
                 ((not (zero? len)))
                 (type (env-look 'content-type))
                 (s (read-body len (current-input-port))))
        (cond ((string-ci= type "application/x-www-form-urlencoded")
               (set! P (alist<-query s)))
              ((string-ci= type "multipart/form-data" 0 19)
               (let ((alist (parse-form (substring/shared type 19) s)))
                 (define (mogrify m)
                   (or (cdr m) (error "badness from parse-form:" m))
                   (if (string? (cdr m))
                       (set! P (cons m P))
                       (call-with-values (lambda () (cdr m))
                         (lambda (filename type headers squeeze)
                           (set! P (acons (car m) filename P))
                           (and pre-squeezed?
                                (let ((value (squeeze substring)))
                                  (set-object-property!
                                   value #:guile-www-cgi
                                   `((#:name . ,(car m))
                                     (#:filename . ,filename)
                                     (#:mime-type . ,type)
                                     (#:raw-mime-headers . ,headers)))
                                  (set-cdr! m value)))
                           (set! U (cons m U))))))
                 (for-each mogrify alist))
               (set! P (reverse! P))
               (set! U (reverse! U)))))
      (set! V (collate P))
      (set! U (collate U))
      (set! C (collate (simple-parse-cookies
                        (or (env-look 'http-cookie) "")))))

    (define (uploads name)
      (and-let* ((pair (assoc name U)))
        (set! U (delq pair U))
        (cdr pair)))

    ;; rv
    (lambda (command . args)
      (define (one)
        (car args))
      (case command
        ((#:init!) (init! args))
        ((#:getenv) (or (env-look (one)) ""))
        ((#:nv-pairs) P)
        ((#:values) (assoc-ref V (one)))
        ((#:value) (and=> (assoc-ref V (one)) car))
        ((#:names) (map car V))
        ((#:form-data?) (not (null? P)))
        ((#:uploads) (uploads (one)))
        ((#:upload) (and=> (uploads (one)) car))
        ((#:cookie-names) (map car C))
        ((#:cookies) (assoc-ref C (one)))
        ((#:cookie) (and=> (assoc-ref C (one)) car))
        (else (error "bad command:" command))))))

(define ONE #f)


;;; Public interface.

;; (Re-)initialize internal data structures.  This must be called before
;; calling any other @samp{cgi:foo} procedure.  For FastCGI, call this
;; ``inside the loop'' (that is, for each CGI invocation).
;;
;; @var{opts} are zero or more symbols that configure the module.
;;
;; @table @code
;; @item uploads-lazy
;; This controls how uploaded files, as per @code{cgi:uploads}
;; and @code{cgi:upload}, are represented.
;; @end table
;;
;; Unrecognized options are ignored.
;;
(define (cgi:init . opts)
  (or ONE (set! ONE (make-ccc)))
  (apply ONE #:init! opts))

;; Return the value of the environment variable associated with @var{key}, a
;; symbol.  Unless otherwise specified below, the return value is a (possibly
;; massaged, possibly empty) string.  The following keys are recognized:
;;
;; @itemize
;; @item server-software-type
;; @item server-software-version
;; @item server-hostname
;; @item gateway-interface
;; @item server-protocol-name
;; @item server-protocol-version
;; @item server-port (integer)
;; @item request-method
;; @item path-info
;; @item path-translated
;; @item script-name
;; @item query-string
;; @item remote-host
;; @item remote-addr
;; @item authentication-type
;; @item remote-user
;; @item remote-ident
;; @item content-type
;; @item content-length (integer, possibly 0)
;; @item http-accept-types (list, possibly empty, of strings)
;; @item http-user-agent
;; @item http-cookie
;; @end itemize
;;
;; Keys not listed above result in an "unrecognized key" error.
;;
(define (cgi:getenv key)
  (ONE #:getenv key))

;; Fetch the list of @code{(name . value)}, in the same order as found
;; in the form data.  A name may appear more than once.  A value is
;; either a string, or @code{#f}.
;;
(define (cgi:nv-pairs)
  (ONE #:nv-pairs))

;; Fetch any values associated with @var{name} found in the form data.
;; Return a list, even if it contains only one element.  A value is
;; either a string, or @code{#f}.  When there are multiple values, the
;; order is the same as that found in the form.
;;
(define (cgi:values name)
  (ONE #:values name))

;; Fetch only the @sc{car} from @code{(cgi:values @var{name})}.
;; Convenient for when you are certain that @var{name} is associated
;; with only one value.
;;
(define (cgi:value name)
  (ONE #:value name))

;; Return a list of variable names in the form.  The order of the
;; list is the same as that found in the form for the first occurance
;; of each variable and each variable appears at most once.  For example,
;; if the form has variables ordered @code{a b a c d b e}, then the
;; returned list would have order @code{a b c d e}.
;;
(define (cgi:names)
  (ONE #:names))

;; Return @code{#t} iff there is form data available.
;;
(define (cgi:form-data?)
  (ONE #:form-data?))

;; Return a list of file contents associated with @var{name},
;; or @code{#f} if no files are available.
;;
;; Uploaded files are parsed by @code{parse-form} (@pxref{form-2-form}).
;; If the @code{uploads-lazy} option is specified to @code{cgi:init}, then
;; the file contents are those directly returned by @code{form-2-form}.
;; If unspecified, the file contents are strings with the object property
;; @code{#:guile-www-cgi} whose value is an alist with the following keys:
;;
;; @table @code
;; @item #:name
;; identical to @var{name} (sanity check)
;;
;; @item #:filename
;; original/suggested filename for this bunch of bits
;;
;; @item #:mime-type
;; something like "image/jpeg"
;;
;; @item #:raw-mime-headers
;; the MIME headers before parsing
;; @end table
;;
;; Note that the string's object property and the keys are all keywords.
;; The associated values are strings.
;;
;; Unless @code{uploads-lazy} is specified (to @code{cgi:init}),
;; @code{cgi:uploads} can only be called once per particular @var{name}.
;; Subsequent calls return @code{#f}.  Caller had better hang onto the
;; information, lest the garbage man whisk it away for good.  This is
;; done to minimize the amount of time the file is resident in memory.
;;
(define (cgi:uploads name)
  (ONE #:uploads name))

;; Fetch the first file associated with form var @var{name}.  Can only be
;; called once per @var{name}, so the caller had better be sure that
;; there is only one file associated with @var{name}.  Use @code{cgi:uploads}
;; if you are unsure.
;;
(define (cgi:upload name)
  (ONE #:upload name))

;; Return a list of cookie names.
;;
(define (cgi:cookie-names)
  (ONE #:cookie-names))

;; Fetch any cookie values associated with @var{name}.  Return a list of
;; values in the order they were found in the HTTP header, which should
;; be the order of most specific to least specific path associated with
;; the cookie.  If no cookies are associated with @var{name}, return
;; @code{#f}.
;;
(define (cgi:cookies name)
  (ONE #:cookies name))

;; Fetch the first cookie value associated with @var{name}.
;;
(define (cgi:cookie name)
  (ONE #:cookie name))

;;; (www cgi) ends here
