(use-modules (ice-9 rdelim))

(define (trim str)
  (substring str 3 (- (string-length str) 1)))

(define (get-thing stream)
  (let ((line (read-line stream nil)))
    (trim line)))

(define (readlines filename)
  (call-with-input-file filename
    (lambda (p)
      (let loop ((line (read-line p))
                 (result '()))
        (if (eof-object? line) (reverse result)
            (loop (read-line p) (cons line result)))))))

(define triples
  (do ((lines (readlines "TR"))
       (accum '()))
      ((null? lines) (reverse accum))
    (let ((s (trim (car lines)))
	  (p (trim (cadr lines)))
	  (o (trim (caddr lines))))
      (set! accum (cons (list s p o) accum))
      (set! lines (cddddr lines)))))

; Let's just verify that filter works the way I expect.
; Yup, it does.
;
;(display
; (filter (lambda (x) (= (modulo x 2) 0))
;	 '(1 2 3 4 5 6 7 8)))
;(newline)  ->  (2 4 6 8)

;; (display triples) (newline)

;; (display (list "abc" "def" "ghi")) (newline)

(define (s-search s)
  (filter (lambda (x) (equal? (car x) s))
	  triples))

(define (p-search p)
  (filter (lambda (x) (equal? (cadr x) p))
	  triples))

(define (o-search o)
  (filter (lambda (x) (equal? (caddr x) o))
	  triples))

(define Canon
  "http://products.semweb.bestbuy.com/company.rdf#Manufacturer_Canon")

(display (o-search Canon))
(newline)


; (match-pattern "?x" "?y" Canon)
