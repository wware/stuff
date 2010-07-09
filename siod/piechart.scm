#!/usr/local/bin/siod -v0,-m3 -*-mode:lisp-*-

;; Purpose:
;;  produce pie chart graphics entirely from the query string portion
;;  of the URL.
;;  
;; Usage: /cgi/pichart.scm?radius=50&red=10&green=20&blue=30
;;
;; Performance notes:
;;  Takes 0.080 cpu seconds for case "304 Not Modified"
;;  Takes 0.250 cpu seconds for a typical pie charts.
;;  That makes the "Not Modified" case important for network bandwith
;;  but not a dramatic win on the CPU side. 

(define (main)
  (let ((if-modified-since (getenv "HTTP_IF_MODIFIED_SINCE"))
	(no-cache (and (string-search "no-cache"
				      (or (getenv "HTTP_PRAGMA") ""))
		       t)))
    (if (or no-cache (not if-modified-since))
	(let ((result (piechart-gif 
		       (mapcar (lambda (x)
				 (let ((u (strbreakup x "=")))
				   (if (not (cdr u))
				       (intern (car u))
				     (list (intern (car u))
					   (string->number (cadr u))))))
			       (strbreakup (or (getenv "QUERY_STRING") "")
					   "&")))))
	  (writes nil
		  "Content-type: image/gif\n"
		  "Last-modified: " (http-date (realtime)) "\n"
		  "Content-length: " (cadr result) "\n"
		  "Cpu-time: " (* 1000 (car (runtime))) "\n"
		  "\n")
	  (fwrite result))
      (writes nil
	      "Status: 304 Not Modified\n"
	      ;; might not really need this Last-modified field.
	      "Last-modified: " if-modified-since  "\n"
	      "Cpu-time: " (* 1000 (car (runtime))) "\n"
	      "\n"))))

(define *color-name-table*
  ;; I should be invoking a utility like the X window color table
  ;; using dbm routes to access /usr/lib/X11/rgb
  '((red 255 0 0)
    (green 0 250 0)
    (blue 0 0 255)
    (white 255 255 255)
    (black 0 0 0)
    (yellow 255 255   0)
    (orange 255 165   0)
    (purple 160  32 240)
    (brown 165  42  42)
    (pink 255 192 203)
    (cyan 0 255 255)
    (magenta 255 0 255)
    (gray 190 190 190)
    (aquamarine 127 255 212)		
    (chartreuse 127 255   0)		
    (gold 255 215   0) 		
    (sienna 160  82  45)		
    (hotpink 255 105 180)		
    (violet 238 130 238)		
    (azure 240 255 255)))

(define (color-name-table x)
  (cdr (assq x *color-name-table*)))

(define (radians->degrees x)
  (set! x (fmod x (* 2 *pi*)))
  (if (< x 0)
      ;; gdImageArc doesn't like negative numbers
      (set! x (+ x (* 2 *pi*))))
  (* (/ x (* 2 *pi*)) 360))

(define (piechart-gif args)
  (let ((im nil)
	(colors nil)
	(radius 50)
	(start-theta (- 0.0 (/ *pi* 2)))
	(end-theta nil)
	(nofill nil))
    (if (not (symbol-bound? 'gdImageCreate))
	(require-so (so-ext 'gd)))
    (if (and (number? (cadr (assq 'radius args)))
	     (> (trunc (cadr (assq 'radius args))) 4)
	     ;; netscape will behave badly on gif images which are large.
	     (< (trunc (cadr (assq 'radius args))) 250))
	(set! radius (trunc (cadr (assq 'radius args)))))
    (set! im (gdImageCreate (* radius 2) (* radius 2)))
    (set! colors (list (list 'white 
			     (apply gdImageColorAllocate
				    (cons im
					  (color-name-table 'white))))))
    (if (or (memq 'interlace args)
	    (and (assq 'interlace args)
		 (not (eqv? 0 (cadr (assq 'interlace args))))))
	(gdImageInterlace im 1))
    (if (or (memq 'transparent args)
	    (and (assq 'transparent args)
		 (not (eqv? 0 (cadr (assq 'transparent args))))))
	(gdImageColorTransparent im 0))
    (if (or (memq 'nofill args)
	    (and (assq 'nofill args)
		 (not (eqv? 0 (cadr (assq 'nofill args))))))
	(set! nofill t))
    (if (number? (cadr (assq 'start args)))
	(set! start-theta (+ start-theta
			     (* (* 2 *pi*)
				(/ (cadr (assq 'start args))
				   100)))))
    (mapcar (lambda (slice)
	      (if (and slice
		       (pair? slice)
		       (color-name-table (car slice))
		       (number? (cadr slice))
		       (> (cadr slice) 0))
		  (let ((ci (cadr (assq (car slice) colors))))
		    (if (not ci)
			(begin (set! ci (apply gdImageColorAllocate
					       (cons im
						     (color-name-table
						      (car slice)))))
			       (set! colors (cons (list (car slice) ci)
						  colors))))
		    ;; now we actually have to do some real work in
		    ;; the form of analytic geometry.
		    (set! end-theta (+ start-theta
				       (* (/ (cadr slice) 100.0)
					  (* 2 *pi*))))
		    (gdImageArc
		     im
		     radius radius ;; center
		     (* radius 2) (* 2 radius) ;; width height
		     (radians->degrees start-theta)
		     (radians->degrees end-theta)
		     ci)
		    (gdImageLine im
				 radius radius
				 (+ radius (* radius (cos start-theta)))
				 (+ radius (* radius (sin start-theta)))
				 ci)
		    (gdImageLine im
				 radius radius
				 (+ radius (* radius (cos end-theta)))
				 (+ radius (* radius (sin end-theta)))
				 ci)
		    (or nofill
			(gdImageFill im 
				     (+ radius
					(* (* radius 0.90)
					   (cos (/ (+ start-theta end-theta)
						   2))))
				     (+ radius
					(* (* radius 0.90)
					   (sin (/ (+ start-theta end-theta)
						   2))))
				     ci))
		    (set! start-theta end-theta))))
	    args)
    (let ((upper-bound (+ 5000 (* (* radius 2) (* radius 2))))
	  (actual nil)
	  (bytes nil))
      (set! bytes (cons-array upper-bound 'byte))
      (set! actual (gdImageGifmem im bytes))
      (list bytes (if (> actual upper-bound) upper-bound actual)))))
