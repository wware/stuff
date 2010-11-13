;; Linear algebra and quaternion operators in Scheme

;; http://pleac.sourceforge.net/pleac_guile/numbers.html

;; Here is an example of using Guile's native arrays to write a matrix
;; multiply routine. Rewritten in native code it would run much quicker.
;; It should also be feasible to write many more functions, like
;; transpose, invert, multiply-matrix-by-vector, etc. A quaternion
;; library should also be doable. The whole thing could be unit-tested
;; to ensure agreement between native versions and pure-Guile reference
;; implementations.

;; linear algebra

(define (matrix-mult m1 m2)
  (let* ((d1 (array-dimensions m1))
         (d2 (array-dimensions m2))
         (m1rows (car d1))
         (m1cols (cadr d1))
         (m2rows (car d2))
         (m2cols (cadr d2)))
    (if (not (= m1cols m2rows))
      (error 'index-error "matrices don't match"))
    (let ((result (make-array 0 m1rows m2cols)))
      (do ((i 0 (1+ i)))
          ((= i m1rows))
        (do ((j 0 (1+ j)))
            ((= j m2cols))
          (do ((k 0 (1+ k)))
              ((= k m1cols))
            (array-set! result (+ (array-ref result i j)
                                  (* (array-ref m1 i k)
                                     (array-ref m2 k j)))
                        i j))))
      result)))

(define (transpose m)
  (let* ((d (array-dimensions m))
         (rows (car d))
         (cols (cadr d)))
    (let ((result (make-array 0 cols rows)))
      (do ((i 0 (1+ i)))
          ((= i rows))
        (do ((j 0 (1+ j)))
            ((= j cols))
	  (array-set! result (array-ref m i j)
		      j i)))
      result)))

(define (vector-to-column-matrix v)
  (let* ((d (array-dimensions v))
         (rows (car d)))
    (let ((cm (make-array 0.0 rows 1)))
      (do ((i 0 (1+ i)))
	  ((= i rows))
	(array-set! cm (array-ref v i)
		    i 0))
      cm)))

(define (column-matrix-to-vector cm)
  (let* ((d (array-dimensions cm))
         (rows (car d))
         (cols (cadr d)))
    (if (not (= cols 1))
	(error 'index-error "too many columns"))
    (let ((v (make-array 0.0 rows)))
      (do ((i 0 (1+ i)))
	  ((= i rows))
	(array-set! v (array-ref cm i 0) i))
      v)))

(define (matrix-times-vector m v)
  (column-matrix-to-vector
   (matrix-mult m (vector-to-column-matrix v))))

(define (determinant m)
  (let* ((d (array-dimensions cm))
         (rows (car d))
         (cols (cadr d)))
    (if (not (= cols rows))
	(error 'index-error "non-square matrix"))
    (if (= rows 1)
	(array-ref m 0 0)
	(let ()
	  (define (submatrix k)
	    (let ((result (make-array 0.0 (1- rows) (1- cols))))
	      (do ((i 0 (1+ i)))
		  ((= i (1- rows)))
		(do ((j 0 (1+ j)))
		    ((= j k))
		  (array-set result (array-ref m (1+ i) j)
			     i j))
		(do ((j (1+ k) (1+ j)))
		    ((= j cols))
		  (array-set result (array-ref m (1+ i) j)
			     i (1- j))))
	      result))
	  ;; todo - finish up
	  ...))))

; still need matrix inverse
; determinant would be good
;   do this recursively
; LDU factoring would be good

;; http://en.wikipedia.org/wiki/Quaternion

(define (q-add q1 q2)
  (vector (+ (array-ref q1 0)
	     (array-ref q2 0))
	  (+ (array-ref q1 1)
	     (array-ref q2 1))
	  (+ (array-ref q1 2)
	     (array-ref q2 2))
	  (+ (array-ref q1 3)
	     (array-ref q2 3))))

(define (q-scale s q)
  (vector  (* (array-ref q 0) s)
	   (* (array-ref q 1) s)
	   (* (array-ref q 2) s)
	   (* (array-ref q 3) s)))

(define (q-subtract q1 q2)
  (q-add q1 (q-scale -1 q2)))

(define (q-multiply q1 q2)
  (let ((q1r (array-ref q1 0))
	(q1i (array-ref q1 1))
	(q1j (array-ref q1 2))
	(q1k (array-ref q1 3))
	(q2r (array-ref q2 0))
	(q2i (array-ref q2 1))
	(q2j (array-ref q2 2))
	(q2k (array-ref q2 3)))
    (vector (- (* q1r q2r)
	       (+ (* q1i q2i)
		  (* q1j q2j)
		  (* q1k q2k)))
	    (- (+ (* q1r q2i)
		  (* q1i q2r)
		  (* q1j q2k))
	       (* q1k q2j))
	    (- (+ (* q1r q2j)
		  (* q1j q2r)
		  (* q1k q2i))
	       (* q1i q2k))
	    (- (+ (* q1r q2k)
		  (* q1i q2j)
		  (* q1k q2r))
	       (* q1j q2i)))))

(define (q-conj q)
  (vector (array-ref q 0)
	  (- (array-ref q 1))
	  (- (array-ref q 2))
	  (- (array-ref q 3))))

(define (q-reciprocal q)
  (let* ((qc (q-conj q))
	 (qq (array-ref (q-multiply q qc) 0)))
    (q-scale (/ 1.0 qq) qc)))

; quaternion versions of exp, log, trig funcs?
