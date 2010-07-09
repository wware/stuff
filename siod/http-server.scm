#!/usr/local/bin/siod -m2 -*-mode:lisp-*-
;; $Id: http-server.scm,v 1.5 1996/03/10 03:26:37 gjc Exp $

;; single-threaded http server for diagnostic purposes.

(define (main)
  (http-server (string->number (or (larg-default (cdddr *args*) 0)
				   "9000"))))

(define (http-server port)
  (let ((s (s-open "0.0.0.0" port 1)))
    (writes nil "*** listening ***\n")
    (*catch 'errobj
	    (while t
	      (let ((a (s-accept s)))
		(writes nil "*** accepted ***\n")
		(http-service-one a))))
    (s-close s)))

(define *key-content-length* "content-length: ")

(define (http-service-one a)
  (let ((line nil)
	(content-length nil))
    (while (and (set! line (s-gets a))
		(not (or (equal? line "\r\n")
			 (equal? line "\n"))))
      (writes nil line)
      (if (and (> (length line) (length *key-content-length*))
	       (equal? *key-content-length*
		       (string-downcase
			(substring line
				   0
				   (length *key-content-length*)))))
	  (set! content-length (string->number
				(substring line
					   (length *key-content-length*))))))
    (if content-length
	(begin (writes nil "*** content " content-length " bytes ***\n")
	       (let ((j 0)
		     (c nil))
		 (while (and (< j content-length)
			     (set! c (s-getc a)))
		   (putc c)
		   (set! j (+ 1 j))))))
    (s-puts (string-append
	     "HTTP/1.0 200 OK\r\n"
	     "Server: Foobar/1.0\r\n"
	     "Content-type: text/plain\r\n"
	     "Date: " (http-date (realtime)) "\r\n"
	     "Last-modified: Saturday, 05-Aug-95 01:03:21 GMT\r\n"
	     "Expires: " (http-date (+ (realtime) 3600)) "\r\n"
	     "Set-Cookie: GJC_1=BEMYGUEST; path=/; "
	     "expires=Wednesday, 09-Nov-99 23:12:40 GMT\r\n"
	     "\r\n"
	     "This server does not have much to say.\r\n")
	    a)
    (s-force-output a)
    (s-close a)
    (writes nil "*** Done ***\n")))
