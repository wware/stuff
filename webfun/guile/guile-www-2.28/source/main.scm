;;; (www main) --- General WWW navigation aids

;; Copyright (C) 2009 Thien-Thi Nguyen
;; Copyright (C) 1997, 2001, 2002, 2003,
;;   2004, 2005 Free Software Foundation, Inc.
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

;; The (www main) module is fully documented in the guile-www.info file.

;;; Code:

(define-module (www main)
  #:export (www:set-protocol-handler!
            www:get
            www:http-head-get)
  #:use-module (www http)
  #:use-module (www url))


(define dispatch-table
  (acons 'http http:get '()))

;; Associate for scheme @var{proto} the procedure @var{handler}.
;; @var{proto} is a symbol, while @var{handler} is a procedure that
;; takes three strings: the host, port and path portions, respectively
;; of a url object.  Its return value is the return value of
;; @code{www:get} (for @var{proto}), and need not be a string.
;;
(define (www:set-protocol-handler! proto handler)
  (set! dispatch-table
        (assq-set! dispatch-table proto handler)))

;; Parse @var{url-string} into portions.  For HTTP, open a connection,
;; retrieve and return the specified document.  Otherwise, consult the
;; handler procedure registered for the particular scheme and apply it
;; to the host, port and path portions of @var{url-string}.  If no such
;; handler exists, signal "unknown URL scheme" error.
;;
(define (www:get url-string)
  (let ((url (url:parse url-string)))
    ;; get handler for this protocol
    (case (url:scheme url)
      ((http) (let ((msg (http:get url)))
                (http:message-body msg)))
      (else
       (let ((handle (assq-ref dispatch-table (url:scheme url))))
         (if handle
             (handle (url:host url)
                     (url:port url)
                     (url:path url))
             (error "unknown URL scheme" (url:scheme url))))))))


;; Parse @var{url-string} into portions; issue an "HTTP HEAD" request.
;; Signal error if the scheme for @var{url-string} is not @code{http}.
;; Optional second arg @code{alist?} non-@code{#f} means return only the
;; alist portion of the HTTP response object.
;;
;;-sig: (url-string [alist?])
;;
(define (www:http-head-get url-string . alist?)
  (let ((url (url:parse url-string)))
    (or (eq? 'http (url:scheme url))
        (error "URL scheme not ‘http’" url-string))
    ((if (or (null? alist?) (not (car alist?)))
         identity
         http:message-headers)
     (http:head url))))

;;; (www main) ends here
