;;; (www server-utils cgi-prep) --- Prepare environment for CGI handoff

;; Copyright (C) 2009 Thien-Thi Nguyen
;; Copyright (C) 2005, 2006 Free Software Foundation, Inc.
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

(define-module (www server-utils cgi-prep)
  #:export (cgi-environment-manager))

(define *env-jamming-methods*
  ;; A list of the form: (VAR1 METHOD1 VAR2 METHOD2 ...).
  ;; METHOD may be a string naming an environment VAR to set,
  ;; or a pair (VAR . PROC), where PROC produces a string or #f.
  `(server-hostname
    "SERVER_NAME"
    gateway-interface
    "GATEWAY_INTERFACE"
    server-port
    ("SERVER_PORT" . ,number->string)
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
    ("CONTENT_LENGTH" . ,(lambda (n) (and n (number->string n))))
    http-user-agent
    "HTTP_USER_AGENT"
    http-cookie
    "HTTP_COOKIE"
    server-software
    "SERVER_SOFTWARE"
    server-protocol
    "SERVER_PROTOCOL"
    http-accept-types
    ("HTTP_ACCEPT" . ,(lambda (ls)
                        (if (or (not ls) (null? ls))
                            ""
                            (apply string-append
                                   (car ls)
                                   (apply append! (map (lambda (x)
                                                         (list ", " x))
                                                       (cdr ls)))))))))

(define *env-jamming*
  (let ((ht (make-hash-table 23)))
    (let loop ((ls *env-jamming-methods*))
      (or (null? ls)
          (begin
            (hashq-set! ht (car ls) (cadr ls))
            (loop (cddr ls)))))
    ht))

;; Return a closure encapsulating @var{initial-bindings}, a list of
;; pairs @code{(@var{name} . @var{value})}, where @var{name} is a symbol
;; listed in the following table, and @var{value} is a string unless
;; otherwise noted.
;;
;; @itemize
;; @item @code{server-hostname}
;; @item @code{gateway-interface}
;; @item @code{server-port} (integer)
;; @item @code{request-method}
;; @item @code{path-info}
;; @item @code{path-translated}
;; @item @code{script-name}
;; @item @code{query-string}
;; @item @code{remote-host}
;; @item @code{remote-addr}
;; @item @code{authentication-type}
;; @item @code{remote-user}
;; @item @code{remote-ident}
;; @item @code{content-type}
;; @item @code{content-length} (integer, or @code{#f})
;; @item @code{http-user-agent}
;; @item @code{http-cookie}
;; @item @code{server-software}
;; @item @code{server-protocol}
;; @item @code{http-accept-types} (list of strings)
;; @end itemize
;;
;; If @var{name} is not recognized, signal "unrecognized key" error.
;; Encapsulation includes @code{@var{name}=@var{value}} formatting.
;;
;; The closure accepts these commands:
;;
;; @table @code
;; @item name value
;; Encapsulate an additional binding.
;; @var{name} and @var{value} are as above.
;;
;; @item #:clear!
;; Drop the additional bindings.  Note that initial bindings can
;; never be dropped (you can always create a new closure).
;;
;; @item #:environ-list
;; Return a list of strings suitable for passing to @code{environ}
;; or as the second argument to @code{execle}.
;; @end table
;;
;; Any other command results in a "bad command" error.
;;
(define (cgi-environment-manager initial-bindings)
  (define (newhash)
    (make-hash-table 23))

  (let ((init-ht (newhash))
        (addl-ht #f))

    (define (reset-addl!)
      (set! addl-ht (newhash)))

    (define (add! ht k v)
      (let* ((method (or (hashq-ref *env-jamming* k)
                         (error "unrecognized key:" k)))
             (lhs (if (pair? method)
                      (car method)
                      method))
             (rhs (if (pair? method)
                      ((cdr method) v)
                      v)))
        (and rhs (hashq-set! ht k (simple-format #f "~A=~A" lhs rhs)))))

    (define (elist ht)
      (hash-fold (lambda (k v acc)
                   (cons v acc))
                 '()
                 ht))

    ;; initialize
    (reset-addl!)
    (for-each (lambda (binding)
                (add! init-ht (car binding) (cdr binding)))
              initial-bindings)

    ;; rv
    (lambda (command . args)
      (if (symbol? command)
          (add! addl-ht command (car args))
          (case command
            ((#:clear!) (reset-addl!))
            ((#:environ-list) (append! (elist init-ht) (elist addl-ht)))
            (else (error "bad command:" command)))))))

;;; (www server-utils cgi-prep) ends here
