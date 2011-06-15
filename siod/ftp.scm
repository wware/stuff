;;-*-mode:lisp-*-
;; name:    ftp
;; purpose: convenient subroutines for handling file transfer protocol
;;
;; $Id: ftp.scm,v 1.3 1996/06/18 20:51:16 gjc Exp $

(require-so (so-ext 'regex))
(require-so (so-ext 'ss))

(define *ftp-debug* t)

(define *ftp-setup-cmds*
  '("TYPE I"))

(define (ftp-fixhostname str)
  (let ((new (string-append str ""))
	(j 0)
	(bad1 (aref "-" 0))
	(bad2 (aref "." 0))
	(good (aref "_" 0)))
    (while (< j (length new))
      (if (or (= (aref new j) bad1)
	      (= (aref new j) bad2))
	  (aset new j good))
      (set! j (+ 1 j)))
    new))

(define (ftp-get-arg host arg)
  (let ((value nil)
	(key (string-append (ftp-fixhostname host) "_" arg)))
    (cond ((and (set! value (getenv key))
		(> (length value) 0))
	   value)
	  ('else
	   (cond ((and (equal? arg "PASS")
		       (symbol-bound? 'getpass))
		  (fflush)
		  (set! value (getpass (string-append key ": "))))
		 ('else
		  (writes nil key ": ")
		  (fflush)
		  (set! value (readline))))
	   (if value (putenv (string-append key "=" value)))
	   value))))

(define (ftp-setup host set)
  (let ((c (set (ftp-open-control host)))
	(result nil))
    (set! result (ftp-cmd-login c
				(lambda () (ftp-get-arg host "USER"))
				(lambda () (ftp-get-arg host "PASS"))
				(lambda () (ftp-get-arg host "ACCT"))))
    (if (or (not result)
	    (= 5 (trunc (/ (car result) 100))))
	(error "ftp-cmd-login" (cadr result)))
    (mapcar (lambda (x)
	      (ftp-cmd c x))
	    *ftp-setup-cmds*)))

(define (ftp-teardown c set)
  (ftp-close-control c)
  (set nil))

(define (ftp.cp host1 host2 cmds)
  (let ((c1 nil)
	(c2 nil))
    (define (setc1 v) (set! c1 v))
    (define (setc2 v) (set! c2 v))
    (*catch 'errobj
	    (ftp-setup host1 setc1)
	    (ftp-setup host2 setc2)
	    (mapcar (lambda (x)
		      (ftp-pasv-copy c1
				     (nth 0 x)
				     c2
				     (nth 2 x)
				     (nth 1 x)))
		    cmds)
	    (ftp-teardown c1 setc1)
	    (ftp-teardown c2 setc2))
    (and c1 (s-close c1))
    (and c2 (s-close c2))))

(define (ftp-send-command c . args)
  (define flag nil)
  (cond (*ftp-debug*
	 (writes nil "" c "---> ")))
  (mapcar (lambda (str)
	    (cond ((not *ftp-debug*))
		  (flag
		   (writes nil "XXXXXXXXXXXXXX"))
		  ('else
		   (writes nil str)
		   (if (equal? str "PASS ") (set! flag t))))
	    (s-puts str c))
	  args)
  (cond (*ftp-debug*
	 (writes nil "\n")
	 (fflush)))
  (s-puts "\r\n" c)
  (s-force-output c))

(define *ftp-response-pat* (regcomp "^([0-9]+)(\-| |$)(.*)$" REG_EXTENDED))

(define (ftp-read-response c)
  (s-force-output c)
  (if *ftp-debug*
      (writes nil c " "))
  (let ((line (s-gets c))
	(match nil)
	(code nil)
	(continue t))
    (cond (*ftp-debug*
	   (if line (writes nil line) (writes nil "*EOF* on command\n"))
	   (fflush)))
    (cond ((not line)
	   nil)
	  ((not (pair? (set! match (regexec *ftp-response-pat* line))))
	   (error "bad response format"))
	  ('else
	   (set! code (string->number (substring line
						 (car (nth 1 match))
						 (cdr (nth 1 match)))))
	   (cond ((not (equal? "-" (substring line
					       (car (nth 2 match))
					       (cdr (nth 2 match)))))
		  (list code
			(substring line
				   (car (nth 3 match))
				   (cdr (nth 3 match)))))
		 ('else
		  (while (and continue
			      (set! line (s-gets c)))
		    (cond (*ftp-debug*
			   (if line (writes nil line)
			     (writes nil "*EOF* on command\n"))
			   (fflush)))
		    (cond ((not line)
			   (set! continue nil))
			  ((not (pair? (set! match (regexec *ftp-response-pat*
							    line)))))
			  ((and (equal? code
					(string->number
					 (substring line
						    (car (nth 1 match))
						    (cdr (nth 1 match)))))
				(not (equal? "-"
					     (substring line
							(car (nth 2 match))
							(cdr (nth 2 match))))))
			   (set! continue nil))))
		  (list code
			(if (pair? match)
			    (substring line
				       (car (nth 3 match))
				       (cdr (nth 3 match)))))))))))
(define (ftp-open-control host)
  (let ((c (s-open host "ftp")))
    (ftp-read-response c)
    c))

(define (ftp-cmd c . args)
  (apply ftp-send-command (cons c args))
  (ftp-read-response c))

(define (ftp-close-control c)
  (ftp-cmd c "QUIT")
  (s-close c))

(define *ftp-pasv-pat*
  (regcomp
   "([0-9]+),([0-9]+),([0-9]+),([0-9]+),([0-9]+),([0-9]+)" REG_EXTENDED))

(define (ftp-cmd-pasv c)
  (let ((res (ftp-cmd c "PASV"))
	(match nil))
    (if (not (and (pair? res) (= 227 (car res))))
	(error "PASV command failed" res))
    (if (and (cadr res)
	     (pair? (set! match (regexec *ftp-pasv-pat* (cadr res)))))
	(mapcar (lambda (x)
		  (substring (cadr res) (car x) (cdr x)))
		(cdr match)))))

(define (ftp-cmd-login c username . args)
  (define (str v)
    (if (or (not v) (string? v))
	v
      (v)))
  (let ((res nil))
    (cond ((or (not (set! res (ftp-cmd c "USER " (str username))))
	       (not (= 331 (car res))))
	   res)
	  ((or (not (set! res (ftp-cmd c "PASS " (str (car args)))))
	       (not (= 332 (car res))))
	   res)
	  ('else
	   (ftp-cmd c "ACCT " (str (cadr args)))))))

(define (ftp-pasv-copy c1 file1 c2 file2 direction)
  (or (memq direction '(-> <-))
      (error "invalid direction specification" direction))
  (let ((p (or (ftp-cmd-pasv c1)
	       (error "pasv command failed to return port info")))
	(res1 nil)
	(res2 nil))
    (set! res2 (ftp-cmd c2
			"PORT "
			(nth 0 p) ","
			(nth 1 p) ","
			(nth 2 p) ","
			(nth 3 p) ","
			(nth 4 p) ","
			(nth 5 p)))
    (or (and (pair? res2) (= 200 (car res2)))
	(error "PORT command failed" res2))
    (cond ((eq? direction '->)
	   (ftp-send-command c1 "RETR " file1)
	   (set! res2 (ftp-cmd c2 "STOR " file2)))
	  ('else
	   (ftp-send-command c1 "STOR " file1)
	   (set! res2 (ftp-cmd c2 "RETR " file2))))
    (set! res1 (ftp-read-response c1))
    (if (and (pair? res1) (= (car res1) 150))
	(set! res1 (ftp-read-response c1)))
    (if (and (pair? res2) (= (car res2) 150))
	(set! res2 (ftp-read-response c2)))
    (list res1 res2)))

