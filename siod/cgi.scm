;;-*-mode:lisp-*-

;; $Id: cgi.scm,v 1.2 1996/05/09 15:55:59 gjc Exp $

;; some more functions useful in CGI programming.
;; Usage:
;; #!/usr/local/lib/siod -v0,-m3
;; (require'cgi.scm)
;;
;; The -m3 flag allows error signalling within the application
;; to be meaningful, especially when the output of the content-type
;; header is delayed until content is ready to transmit.

(define (parse-query-string str)
  (mapcar (lambda (x)
	    (and x (cons (intern (car x)) (cdr x))))
	  (mapcar (lambda (x)
		    (mapcar url-decode
			    (strbreakup x "=")))
		  (strbreakup str "&"))))

(define (read-content-alist)
  (or (equal? "application/x-www-form-urlencoded"
	      (or (getenv "CONTENT_TYPE")
		  (error "no CONTENT_TYPE specified in HTTP transaction.")))
      (error "Incorrect CONTENT_TYPE in HTTP transaction."
	     (getenv "CONTENT_TYPE")))
  (parse-query-string (or (and (getenv "CONTENT_LENGTH")
			       (fread (string->number
				       (getenv "CONTENT_LENGTH"))))
			  "")))
