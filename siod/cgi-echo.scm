#!/usr/local/bin/siod -v0,-m2 -*-mode:lisp-*-

;; Yes, this is a CGI script written in Scheme. Echo the environment
;; back to the client.
;; 15-MAR-95 George Carrette.
;; $Id: cgi-echo.scm,v 1.4 1996/03/12 16:04:19 gjc Exp $

(writes nil "Content-type: text/plain\r\n\r\n")

(define (main)
  (let ((do-fflush (and (getenv "PATH_INFO")
		     (string-search "flush"
				    (getenv "PATH_INFO"))))
	(line-printer nil))
    (if do-fflush
	(begin (writes nil "fflushing after each string\r\n")
	       (set! line-printer
		     (lambda (x) (print x) (fflush))))
      (set! line-printer print))
    (writes nil (length *args*) " arguments:\r\n")
    (mapcar line-printer *args*)
    (writes nil "Environment:\r\n")
    (mapcar line-printer *env*)

    (if (getenv "CONTENT_LENGTH")
	(let ((n (string->number (getenv "CONTENT_LENGTH"))))
	  (writes nil
		  n " bytes of encoded content.\r\n")
	  ;; "&" is for url encoded data.
	  (mapcar
	   (lambda (element)
	     (if (> (length element) 10)
		 (mapcar line-printer (strbreakup element "%0D%0A"))
	       (line-printer element)))
	   (strbreakup (or (fread n) "") "&"))))

    (writes nil
	    "\r\n\r\nquery took "
	    (* (car (runtime)) 1000)
	    " milliseconds seconds cpu time.\r\n")))

