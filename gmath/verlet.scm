;; Verlet integration

(let* ((dt 0.00001)
       (altitude 0.0)
       (next-altitude (* dt 10))
       (gravity -9.8))
  (do ((t 0 (+ t dt)))
      ((>= t 2.041))
    (display t) (display " ") (display altitude) (newline)
    (let ((old-altitude altitude))
      (set! altitude next-altitude)
      (set! next-altitude (+ (* 2 altitude)
			     (- old-altitude)
			     (* gravity dt dt))))))
