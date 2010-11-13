#!/usr/bin/guile -s
!#

(load "math.scm")

(define (nearly-equal? x y thresh)
  (let* ((thresh-sq (* thresh thresh)))
    (cond
     ((and (real? x) (real? y))
      ;; the complex clause would get this, but this is faster
      ;; for the majority of numbers, which are real
      (let ((d (- x y)))
	(< (* d d) thresh-sq)))
     ((and (complex? x) (complex? y))
      (let* ((d (- x y))
	     (dconjugate (- (real-part d)
			    (* 0+1i (imag-part d)))))
	(< (real-part (* d dconjugate)) thresh-sq)))
     ((and (string? x) (string? y))
      (equal? x y))
     ((and (list? x) (list? y))
      (if (not (= (length x) (length y)))
	  #f
	  (do ((i 0 (1+ i))
	       (same #t))
	      ((or (= i (length x))
		   (not same)) same)
	    (set! same (nearly-equal?
			(list-ref x i)
			(list-ref y i)
			thresh)))))
     ((and (array? x) (array? y))
      (let ((d1 (array-dimensions x))
	    (d2 (array-dimensions y)))
	(if (not (equal? d1 d2))
	    #f
	    (let ((same #t))
	      (array-for-each
	       (lambda (u v)
		 (if (not (nearly-equal? u v thresh))
		     (set! same #f)))
	       x y)
	      same))))
     (#t #f))))

(define (run-test-case tc)
  (let ((name (car tc))
	(result (eval (cadr tc) (current-module)))
	(expected (caddr tc)))
    (if (not (nearly-equal? result expected 1.0e-6))
	(let ()
	  (display name)
	  (newline)
	  (display "     Got: ")
	  (display result)
	  (newline)
	  (display "Expected: ")
	  (display expected)
	  (newline)))))

;; regression tests
(map run-test-case
     '(("Addition"
	(+ 1 2)
	3)

       ("Transpose"
	(transpose '#2((3 2 3) (5 9 8)))
	#2((3 5) (2 9) (3 8)))

       ("Matrix multiply"
	(matrix-mult '#2((3 2 3) (5 9 8)) '#2((4 7) (9 3) (8 1)))
	#2((54 30) (165 70)))

       ("Matrix multiply"
	(matrix-mult '#2((4 7) (9 3) (8 1)) '#2((3 2 3) (5 9 8)))
	#2((47 71 68) (42 45 51) (29 25 32)))

       ("matrix-times-vector"
	(let* ((theta 0.2)
	       (c (cos theta))
	       (s (sin theta))
	       (S (- s))
	       (a (list->array '(0 0)
				(list (list c 0 s)
				      (list 0 1 0)
				      (list S 0 c)))))
	  (matrix-times-vector a '#(4 3 5)))
	#(4.913613 3 4.105656))

       ("q-add"
	(q-add '#(1 2 3 4) '#(5 6 7 8))
	#(6 8 10 12))

       ("q-subtract"
	(q-subtract '#(1 2 3 4) '#(5 6 7 8))
	#(-4 -4 -4 -4))

       ("q-multiply"
	(q-multiply '#(1 2 3 4) '#(5 6 7 8))
	#(-60 12 30 24))

       ("q-reciprocal"
	(let ((q '#(4.03 0.71283 -3.15 6)))
	  (q-multiply q (q-reciprocal q)))
	#(1.0 0.0 0.0 0.0))))
