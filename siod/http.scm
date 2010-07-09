;;-*-mode:lisp-*-
;; 15-MAR-95. George Carrette. GJC@DELPHI.COM 
;;            A hyper-text-protocol (HTTP) client.
;; $Id: http.scm,v 1.2 1995/12/06 14:13:50 gjc Exp $

(define (http-get-file server url hfile dfile)
  (let ((s (s-open (if (pair? server) (car server) server)
		   (if (pair? server) (car (cdr server)) 80)))
	(hf (and hfile (not (pair? hfile)) (fopen hfile "w")))
	(df (and dfile (not (pair? dfile))) (fopen dfile "w"))
	(line nil)
	(c nil)
	(hdr nil))
    (if (pair? url)
	(while url
	  (s-puts (car url) s)
	  (s-puts "\r\n" s)
	  (set! url (cdr url)))
      (s-puts (string-append "GET " url " HTTP/1.0\r\n\r\n")
	      s))
    (s-force-output s)
    (or hf
	(eqv? hfile t)
	(puts "---Response Header---\n"))
    (while (and (set! line (s-gets s))
		(not (or (equal? line "\r\n")
			 (equal? line "\n"))))
      (puts line hf)
      (set! hdr (cons line hdr)))
    (and hf (fclose hf))
    (or df
	(puts "---Response Data---\n"))
    (while (set! c (s-getc s)) (putc c df))
    (and df (fclose df))
    (s-close s)
    (nreverse hdr)))

(define (http-post server url data hfile dfile)
  (http-get-file server
		 (list (string-append "POST "url " HTTP/1.0")
		       "User-Agent: Hyper Text Query System in Lisp"
		       "Content-type: application/x-www-form-urlencoded"
		       (string-append "Content-length: "
				      (number->string (string-length data)))
		       ""
		       data)
		 hfile
		 dfile))

(define (url-encode-form-data . l)
  (define (loop x)
    (if (not x)
	nil
      (if (not (cdr x))
	  (error "odd number of arguments")
	(let ((rest (loop (cddr x))))
	  (cons (car x)
		(cons "="
		      (cons (if (number? (cadr x))
				(number->string (cadr x))
			      (url-encode (cadr x)))
			    (if rest
				(cons "&" rest)))))))))
  (apply string-append (loop l)))
