;; -*-mode:lisp-*-

;; Usage:
;;         siod pop3.scm "(pop3-demo)"
;;

(define (pop3-demo . rest)
  (let ((filename nil)
	(host (car rest))
	(username (cadr rest))
	(password (caddr rest)))
    (cond ((and (not host)
		(not (set! host (getenv "POP3_SERVER"))))
	   (writes nil "pop3 server: ")
	   (fflush nil)
	   (set! host (readline))))
    (cond ((and (not username)
		(not (set! username (getenv "POP3_USERNAME"))))
	   (writes nil "username: ")
	   (fflush nil)
	   (set! username (readline))))
    (cond ((and (not password)
		(not (set! password (getenv "POP3_PASSWORD"))))
	   (set! password (getpass "password: "))))
    (set! filename (string-append (or (getenv "HOME") ".")
				  "/"
				  username
				  ".mail"))
    (pop3-load host username password
	       (lambda (data)
		 (writes nil (length data)
			 " messages being saved to " filename "\n")
		 (save-pop3-mail filename data)
		 (writes nil "done.\n")))
    nil))

(require-so (so-ext'ss))

(define *pop3-debug* nil)

(define (pop3-open host)
  (let ((s (s-open host "pop3"))
	(response nil))
    (cond ((not (set! response (pop3-response s nil)))
	   (s-close s)
	   (error "pop3 response eof"))
	  ((string? response)
	   (error "pop3" response))
	  ('else
	   s))))

(define (pop3-line str)
  (substring str
	     0
	     (or (string-search "\r" str)
		 (string-search "\n" str))))


(define (pop3-response s ok-multi)
  (let ((line (s-gets s))
	(result nil))
    (if *pop3-debug* (writes nil line))
    (cond ((not line)
	   nil)
	  ((and (substring-equal? "+" line 0 1) ok-multi)
	   (set! result (list (strbreakup (pop3-line line) " ")))
	   (while (and (set! line (s-gets s))
		       (not (equal? "." (set! line (pop3-line line)))))
	     (if *pop3-debug* (writes nil line "\n"))
	     (set! result (cons (if (substring-equal? "." line 0 1)
				    (substring line 1)
				  line)
				result)))
	   (nreverse result))
	  ((substring-equal? "+" line 0 1)
	   (strbreakup (pop3-line line) " "))
	  (line))))

(define (pop3-cmd s . l)
  (mapcar (lambda (x)
	    (if *pop3-debug* (writes nil x))
	    (s-puts (if (number? x) (number->string x) x) s))
	  l)
  (if *pop3-debug* (writes nil "\n"))
  (s-puts "\r\n" s)
  (s-force-output s))

(define (pop3-err-check result)
  (if (string? result)
      (error "pop3" result)))

(define (pop3-login s username password)
  (pop3-cmd s "USER " username)
  (pop3-err-check (pop3-response s nil))
  (pop3-cmd s "PASS " password)
  (pop3-err-check (pop3-response s nil)))

(define (pop3-quit s)
  (pop3-cmd s "QUIT")
  (pop3-response s nil)
  (s-close s))

(define (pop3-list s)
  (pop3-cmd s "LIST")
  (let ((response (pop3-response s t)))
    (mapcar (lambda (x)
	      (mapcar string->number (strbreakup x " ")))
	    (if (pair? response) (cdr response) nil))))

(define (pop3-delete-message s l)
  (cond ((not l)
	 nil)
	((pair? l)
	 (mapcar (lambda (x) (pop3-delete-message s x)) l))
	('else
	 (pop3-cmd s "DELE " l)
	 (pop3-response s nil))))

(define (pop3-get-message s l)
  (cond ((not l)
	 nil)
	((pair? l)
	 (mapcar (lambda (x) (pop3-get-message s x)) l))
	('else
	 (pop3-cmd s "RETR " l)
	 (pop3-response s t))))

(define (pop3-load host username password deletep)
  (let ((s (pop3-open host))
	(message-numbers nil)
	(messages nil))
    (pop3-login s username password)
    (set! message-numbers (mapcar car (pop3-list s)))
    (set! messages (mapcar (lambda (x) (and (pair? x) (cdr x)))
			   (pop3-get-message s message-numbers)))
    (or (symbol? deletep)
	(deletep messages))
    (if deletep (pop3-delete-message s message-numbers))
    (pop3-quit s)
    messages))

(define (save-pop3-mail filename data)
  (let ((f (fopen filename "a+"))
	(l1 data)
	(l2 nil))
    (while l1
      (set! l2 (car l1))
      (set! l1 (cdr l1))
      (while l2
	(cond ((equal? (car l2) ".")
	       (writes f "..\n"))
	      ('else
	       (writes f (car l2) "\n")))
	(set! l2 (cdr l2)))
      (writes f ".\n"))
    (fclose f)))

