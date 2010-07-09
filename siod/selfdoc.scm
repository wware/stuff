#!/usr/local/bin/siod -v01,-m2 -*-mode:lisp-*-

;; usage:
;;
;; selfdoc.scm /usr/local/lib/siod/*.so > selfdoc.txt

(define ++++builtin-type-explain-table
  '((tc_subr_0 "built-in subroutine of 0 arguments.")
    (tc_subr_1 "built-in subroutine of 1 argument.")
    (tc_subr_2 "built-in subroutine of 2 arguments.")
    (tc_subr_2n "built-in subroutine of nary arguments")
    (tc_subr_3 "built-in subroutine of 3 arguments.")
    (tc_lsubr "built-in subroutine 0 or more arguments.")
    (tc_fsubr "built-in special form.")
    (tc_msubr "built-in special form.")))

(define (++++global-id-info)
  (mapcar (lambda (x)
	    (list x t))
	  (qsort (apropos "$Id: ") string-lessp)))

(define (++++global-symbol-info)
  (mapcar (lambda (x)
	    (list x (typeof (symbol-value x))))
	  (qsort (subset (lambda (x)
			   (and (symbol-bound? x)
				(not (memq x '(++++builtin-type-explain-table
					       ++++global-symbol-info
					       ++++display-info
					       ++++main
					       ++++whats-new
					       ++++global-id-info
					       main
					       )))))
			 (apropos ""))
		 string-lessp)))

(define (++++display-info l)
  (mapcar (lambda (info)
	    (let ((kind (or (cadr (assq (cadr info)
					++++builtin-type-explain-table))
			    (string-append "variable, value data type "
					   (if (number? (cadr info))
					       (number->string (cadr info))
					     (cadr info)))))
		  (pict (string-append (car info)))
		  (maxpict 30))
	      (writes nil
		      pict
		      " "
		      (if (> (length pict) maxpict)
			  ""
			(cons-array (- maxpict (length pict)) 'string))
		      " "
		      kind "\n")))
	  l)
  nil)

(define (++++whats-new old new)
  (let ((table (cons-array 1000)))
    (mapcar (lambda (x)
	      (hset table (car x) (cadr x)))
	    old)
    (subset (lambda (x)
	      (not (equal? (href table (car x)) (cadr x))))
	    new)))
 
(define (++++main extensions)
  (define (display-ids l)
    (mapcar (lambda (x)
	      (writes nil ";;; " (car x) "\n"))
	    l))
  (let ((base-info (++++global-symbol-info))
	(new-info nil)
	(base-ids (++++global-id-info))
	(new-ids nil))
    (writes nil ";;; built-in functionality\n")
    (display-ids base-ids)
    (++++display-info base-info)
    (mapcar (lambda (extension)
	      (require-so extension)
	      (writes nil ";;; " extension "\n")
	      (set! new-ids (++++global-id-info))
	      (display-ids (++++whats-new base-ids new-ids))
	      (set! new-info (++++global-symbol-info))
	      (++++display-info (++++whats-new base-info new-info))
	      (set! base-info new-info)
	      (set! base-ids new-ids))
	    extensions)))


(define (main)
  (let ((extensions nil)
	(extension nil)
	(j 0))
    (while (set! extension (larg-default (cdddr *args*) j))
      (cond ((not (equal? (string-append "libsiod" (so-ext))
			  extension))
	     (set! extensions (append extensions (list extension)))))
      (set! j (+ 1 j)))
    (++++main extensions)))

