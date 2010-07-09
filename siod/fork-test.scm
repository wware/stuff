;;-*-mode:lisp-*-
;; $Id: fork-test.scm,v 1.2 1995/12/06 14:13:16 gjc Exp $

(define (fork-test-1)
  (let ((pid (fork)))
    (if pid
	(begin (writes nil
		       "I am parent. Child pid = "
		       pid " quiting.\n")
	       (quit))
      (begin (writes nil
		     "I am child. Sleeping...\n")
	     (sleep 3)
	     (let ((j 0))
	       (while (< j 10)
		 (writes nil "Here I run... " j "\n")
		 (sleep 2)
		 (set! j (+ 1 j))))))))

(define (fork-test-2)
  (while (fork))
  (writes nil "child sleeping...\n")
  (sleep 10)
  (writes nil "quit\n")
  (quit))

(define (wait1 pid opt)
  (writes nil "Waiting on pid " pid "\n")
  (let ((result (wait pid opt)))
    (if (pair? result)
	(writes nil "Got pid " (car result)
		" exit status " (cadr result) "\n"))
    result))

(define (fork-test-3 . a)
  (let ((pid (fork)))
    (if pid
	(while (not (wait1 pid (if (memq 'nohang a) '(WNOHANG))))
	  (sleep 1))
      (begin (writes nil "sleeping child, pid = " (getpid) "\n")
	     (sleep 5)
	     (if (memq 'fault a)
		 (begin (writes nil "going to fault\n")
			(%%%memref 0))
	       (if (memq 'exit a)
		   (begin (writes nil "exiting returning\n")
			  (exit 0))
		 (begin (writes nil "calling quit()\n")
			(quit))))))))

