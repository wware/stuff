;;-*-mode:lisp-*-
;;
;; name:    http-stress.scm
;; purpose: simple random referencing of a list of url's as
;;          fast as we can from a set of forked() processes.
;; Author:  George Carrette GJC@DELPHI.COM
;; Usage:   (get-random-url-test n-cycles) for a single process test.
;;          (fork-random-url-test n-forks n-cycles)
;;          (http-get-test url)
;;          (http-set-basic-authorization username password)
;;          (http-put-test url content-type content)
;;          (main-fork-random-url-test n-forks n-cycles)
;;          (continuos-loop-test time-limit)
;;
;; Example:
;;           siod -v5 http-stress.scm "(get-random-url-test 10)"
;;
;; $Id: http-stress.scm,v 1.9 1996/03/11 20:44:59 gjc Exp $
;;
;; Revision History
;; 16-MAY-95. GJC Initial Creation, rework of previous hacks, adding forking.
;; 17-MAY-95. GJC Add headers for user-agent,authorization,accept.
;; 18-MAY-95. GJC generalize to do POST operations and handle content.
;;
;;
;; Problems: if a file is being loaded with the fork calls in them
;;           the the input stream on the file gets random data in it.
;;           usually resulting in unbound variable error.
;;           The 'work-around' for this is to put a (quit) in the test
;;           script file. The problem goes away if the wait call is
;;           avoided.

;; Note: Need to run this with siod compiled with profiling
;;       so we can perhaps see what routines are taking most of the
;;       cpu time. Is it s-drain? If we had a profile-all subrs mode
;;       in siod that would be cute.

(define (srandom-now)
  (srandom (fmod (* 1.0e4 (realtime)) 1.0e5)))

(define *http-server-host* "www.foobar.com")
(define *http-server-port* 80)
(define *http-user-agent* "DELPHI-http-stress/1.0")
(define *http-user-authorization* nil)
(define *http-accept* "*/*, image/gif, image/jpeg")
(define *http-extra-headers* nil)

(define *http-url-list*
  '("/" "/foo/" "/bar/"))

(define *main-url-verify* nil)

(define (http-set-basic-authorization username password)
  (cond ((or (not username) (not password))
	 (set! *http-user-authorization* nil))
	('else
	 (set! *http-user-authorization*
	       (string-append "Basic "
			      (base64encode (string-append username
							   ":"
							   password)))))))

(define (http-get-test url)
  (http-test url "GET" nil nil))

(define (http-put-test url content-type content)
  (http-test url "POST" content-type content))

(define (http-open url operation content-type content)
  (let ((s (s-open *http-server-host* *http-server-port*))
	(response-headers nil)
	(line nil))
    (s-puts
     (string-append operation " " url " HTTP/1.0\r\n")
     s)
    (if *http-accept*
	(s-puts
	 (string-append "Accept: " *http-accept* "\r\n")
	 s))
    (if *http-user-agent*
	(s-puts
	 (string-append "User-Agent: "
			*http-user-agent*
			"\r\n")
	 s))
    (if *http-user-authorization*
	(s-puts
	 (string-append "Authorization: "
			*http-user-authorization*
			"\r\n")
	 s))
    (mapcar (lambda (h)
	      (s-puts (car h) s)
	      (s-puts ": " s)
	      (s-puts (cadr h) s)
	      (s-puts "\r\n" s))
	    *http-extra-headers*)
    (if content-type
	(s-puts
	 (string-append "Content-type: "
			content-type
			"\r\n")
	 s))
    (if content
	(s-puts
	 (string-append "Content-length: "
			(number->string (string-length content))
			"\r\n")
	 s))
    (s-puts "\r\n" s)
    (if content
	(s-puts content s))
    (s-force-output s)
    (while (and (set! line (s-gets s))
		(not (or (equal? line "\r\n")
			 (equal? line "\n"))))
      (set! response-headers (cons (string-trim line) response-headers)))
    (cons s (nreverse response-headers))))

(define (http-test url operation content-type content)
  (let ((l (http-open url operation content-type content))
	(s nil))
    (set! s (car l))
    (if (> (verbose) 6)
	(let ((c nil))
	  (while (set! c (s-getc s)) (putc c)))
      (s-drain s))
    (s-close s)
    (cdr l)))

(define (http url operation content-type content)
  (let ((l (http-open url operation content-type content))
	(s nil)
	(line nil)
	(result nil))
    (set! s (car l))
    (while (set! line (s-gets s))
      (set! result (cons line result)))
    (s-close s)
    (nconc (cdr l)
	   (cons nil (nreverse result)))))

(define (get-random-url)
  (http-get-test (nth (random (length *http-url-list*))
		      *http-url-list*)))

(define (get-random-url-test n)
  (srandom-now)
  (let ((j 0))
    (while (< j n)
      (get-random-url n)
      (set! j (+ 1 j)))))

(define (fork-random-url-test n-forks n-cycles)
  (let ((children nil)
	(j 0)
	(pid nil)
	(info nil))
    (while (< j n-forks)
      (if (set! pid (fork))
	  (begin (if (> (verbose) 1)
		     (writes nil "PARENT: Started " pid "\n"))
		 (set! children (cons pid children)))
	(begin (if (> (verbose) 1)
		   (writes nil "CHILD: " (getpid) "\n"))
	       (set! j n-forks)))
      (set! j (+ j 1)))
    (if pid
	(begin (writes nil "PARENT: waiting on " (length children)
		       " children.\n")
	       ;; this will run until it gets an error.
	       (while children
		 (set! info (wait-for-child))
		 (set! children (subset (lambda (c)
					  (not (eqv? c (car info))))
					children))
		 (writes nil "PARENT: wait returned ["
			 (car info) "," (cadr info) "] "
			 (length children) " left\n")))
      (begin (if (> (verbose) 1)
		 (writes nil "CHILD: " (getpid)
			 " random fetch " n-cycles
			 " times through " (length *http-url-list*)
			 " possible urls.\n"))
	     (get-random-url-test n-cycles)
	     (if (> (verbose) 1)
		 (writes nil "CHILD: " (getpid) " exiting.\n"))
	     (exit 0)))))

(define (wait-for-child)
  (let ((result nil))
    (while (not (set! result (wait nil '(WNOHANG))))
      (if (> (verbose) 2)
	  (begin (writes nil ".") (fflush)))
      (sleep 1))
    result))

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

(if (> (verbose) 4)
    (trace http-get-test))

(define (get-total-runtime)
  (apply +
	 (mapcar (lambda (x)
		   (let ((r (current-resource-usage x)))
		     (apply +
			    (mapcar (lambda (y)
				      (cdr (assq y r)))
				    '(utime stime)))))
		 '(SELF CHILDREN))))

(define (main-fork-random-url-test n-forks n-hits-per-fork)
  (if *main-url-verify*
      (begin (mapcar (lambda (sym)
		       (writes nil sym " = " (eval sym) "\n"))
		     '(*http-server-host*
		       *http-server-port*
		       *http-user-agent*
		       *http-user-authorization*
		       *http-accept*))
	     (writes nil "Verification of url access:\n")
	     (mapcar (lambda (x)
		       (writes nil x "\n")
		       (mapcar (lambda (line)
				 (writes nil " " line "\n"))
			       (http-get-test x)))
		     *http-url-list*)
	     (writes nil "********************************\n")))
  (let ((before-real nil)
        (before-run nil)
	(after-real nil)
	(after-run nil)
        (test-real nil)
        (test-run nil))
    (set! before-real (realtime))
    (set! before-run (get-total-runtime))
    (fork-random-url-test n-forks n-hits-per-fork)
    (set! after-real (realtime))
    (set! after-run (get-total-runtime))
    (set! test-real (- after-real before-real))
    (set! test-run (- after-run before-run))
    (writes nil
	    "********************************\n"
	    "Test start: " (unix-ctime before-real) "\n"
	    "Test ended: " (unix-ctime after-real) "\n"
            "test took " test-real " seconds realtime, "
            test-run " cpu seconds. ("
	    (* 100 (/ test-run test-real)) "%)\n"
            (/ test-real (* n-forks n-hits-per-fork))
            " realtime seconds per hit.\n"
	    (* n-forks n-hits-per-fork)
	    " total hits. " (/ (* n-forks n-hits-per-fork) test-real)
	    " hits per second.\n")))


(define (continuos-loop-test time-limit)
  (let ((grand-start-time (realtime))
	(before-real nil)
        (before-run nil)
	(after-real nil)
	(after-run nil)
        (test-real nil)
        (test-run nil)
	(l nil))
    (while (or (not time-limit)
	       (< (realtime) (+ time-limit grand-start-time)))
      (set! before-real (realtime))
      (set! before-run (get-total-runtime))
      (mapcar http-get-test *http-url-list*)
      (set! after-real (realtime))
      (set! after-run (get-total-runtime))
      (set! test-real (- after-real before-real))
      (set! test-run (- after-run before-run))
      (writes nil "["
	      (substring (unix-ctime before-real) 11 19) ","
	      (substring (unix-ctime after-real) 11 19)
	      "] "
	      (trunc (/ (length *http-url-list*) test-real))
	      " hits per second. "
	      (trunc (/ (* 100 test-run) test-real))
	      "% cpu.\n"))))
