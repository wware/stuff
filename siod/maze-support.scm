;;-*-mode:lisp-*-
;; $Id: maze-support.scm,v 1.2 1995/12/06 14:13:50 gjc Exp $

;; implements a maze with a time limit on path choices.
;; 16-MAR-95 George Carrette <gjc@delphi.com>

(or (symbol-bound? '*maze*) (define *maze* (cons-array 10)))

(define define-room 'define-room-macro)

(or (symbol-bound? '*path-info*)
    (define *path-info* (getenv "PATH_INFO")))
(or (symbol-bound? '*script-prefix*)
    (define *script-prefix* (string-append (or (getenv "SCRIPT_NAME") "")
					   "/")))

(define (define-room-macro form)
  (hset *maze* (nth 1 form) (cdr form))
  (list 'quote (nth 1 form)))

(define-room no-such-room
  (narrative
   "I'm sorry, but the maze is evidently still under construction."
   "Go back now!"))

(define-room sorry-ran-out-of-time
  (narrative "You ran out of time to make your choice of paths,"
	     "the only thing you can do from here is to start over.")
  (paths (go-to-starting-room starting-room)))

(define-room invalid-uri
  (narrative "An invalid Universal Resource Identifier was detected."
	     "Either this maze simulation program is broken"
	     "or you, the client, have attempted to construct"
	     "your own URI into the middle of the maze.")
  (paths (go-to-starting-room starting-room)))

(define (start-html)
  (put-lines "Content-type: text/html"
	     ""
	     "<!doctype html public \"-//IETF//DTD HTML//EN\">"
	     "<html>"))

(define (put-lines . l)
  (while l
    (if (pair? (car l))
	(apply put-lines (car l))
      (begin (puts (if (number? (car l))
		       (number->string (car l))
		     (or (car l) "")))
	     (puts "\r\n")))
    (set! l (cdr l))))

(define (run-maze)
  (start-html)
  (if (not *path-info*)
      (display-room 'starting-room nil)
    (let ((path (strbreakup *path-info* "/")))
      ;; need more error checking on this
      (take-path (intern (nth 1 path))
		 (intern (nth 2 path))
		 (string->number (nth 3 path) 10)
		 (string->number (nth 4 path) 10))))
  (put-lines "</body>"
	     "</html>"))

(define (head title)
  (put-lines "<head><title>"
	      title
	      "</title></head>"
	      "<body>"))

(define (display-room r time-left)
  (let ((desc (or (href *maze* r) (href *maze* 'no-such-room))))
    (let ((time-limit (cadr (assq 'time-limit desc))))
      (head (string-append "Maze " (car desc)))
      (if time-left
	  (put-lines "You had " time-left " seconds left.<HR>"))
      (if (cdr (assq 'narrative desc))
	  (put-lines (cdr (assq 'narrative desc)) "<HR>"))
      (if time-limit
	  (put-lines "You have " time-limit
		     "seconds in which to choose the path you want to take"
		     "out of this room."
		     "<HR>"))
      (if (cadr (assq 'picture desc))
	  (put-lines
	   (string-append "<img align=left "
			  "hspace=15 "
			  "src=\""
			  (cadr (assq 'picture desc))
			  "\">")
	   ""))
      (let ((paths (cdr (assq 'paths desc))))
	(if (not paths)
	    (put-lines "There is no way out of this room."
		       "Go back if you still have time.")
	  (while paths
	    (puts "<h4><a href=\"")
	    (puts (make-choice-href (car desc)
				    (caar paths)
				    time-limit))
	    (puts "\">")
	    (puts (caar paths))
	    (puts "</a></h4>\r\n")
	    (set! paths (cdr paths))))))))

(define (take-path from-room through-door time-limit signature)
  (if (eqv? signature
	    (data-signature (make-choice-data from-room
					      through-door
					      (if (eqv? time-limit -1)
						  nil
						time-limit))))
      (let ((time-to-spare (and (not (eqv? time-limit -1))
				(- time-limit (unix-time)))))
	(if (and time-to-spare
		 (< time-to-spare 0))
	    (display-room 'sorry-ran-out-of-time time-to-spare)
	  (display-room
	   (or (cadr (assq through-door
			   (cdr (assq 'paths (href *maze* from-room)))))
	       'no-such-room)
	   time-to-spare)))
    (display-room 'invalid-uri nil)))

(define (make-choice-data room door time-limit)
  (string-append room
		 "/"
		 door
		 "/"
		 (if time-limit (number->string time-limit 10) "-1")))

(define (data-signature x)
  ;; todo: some sort of validation signature better
  ;; than this sxhash built-in.
  (sxhash x 10000000))

(define (make-choice-href room door time-limit)
  (let ((data (make-choice-data room door
				(if time-limit
				    (+ (unix-time) time-limit)))))
    (string-append *script-prefix*
		   data
		   "/"
		   (number->string (data-signature data) 10))))
    
			     
