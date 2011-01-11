;     Guile Scheme Misc. DB Utils, part of Guile Web
;     Copyright (C) 2002,2003 Clinton Ebadi

;     This program is free software; you can redistribute it and/or modify
;     it under the terms of the GNU General Public License as published by
;     the Free Software Foundation; either version 2 of the License, or
;     (at your option) any later version.

;     This program is distributed in the hope that it will be useful,
;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;     GNU General Public License for more details.

;     You should have received a copy of the GNU General Public License
;     along with this program; if not, write to the Free Software
;     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


;;; this should make using simplesql easier
(define-module (web db)
  #:use-module (srfi srfi-1)
  #:export (db:res->assoc db:numrows? db:fetch-row
	    db:get))

;;; this converts a simplesql list of vectors into a new vector that
;;; contains a bunch of hash tables and the number of hash tables:
;;; #(num-rows [hash table 1] ... [hash table N])
; (define (db:res->assoc db-res)
;   (if (list? db-res)
;     (let ((names 
; 	   (map string->symbol (vector->list (car db-res))))
; 	  (name-len (1+ (vector-length (car db-res))))
; 	  (data (cdr db-res))
; 	  (assoc (make-vector (1- (length db-res)))))
;       (let main-loop ((i 0) (data data))
; 	(cond ((not (null? data))
; 	       (let ((h (make-hash-table name-len))
; 		     (x (car data)))
; 		 (let loop ((i 0) (lst names))
; 		   (cond ((not (null? lst))
; 			  (hashq-create-handle! h
; 						(car lst)
; 						(vector-ref x i))
; 			  (loop (1+ i) (cdr lst)))))
; 		 (vector-set! assoc i h))
; 	       (main-loop (1+ i) (cdr data)))))
;       assoc)
;     #f ))

(define (db:res->assoc db-res)
  (if (list? db-res)
      (let ((vec (list->vector db-res)))
	(vector-set! vec 0
	      (map (lambda (x) (string->symbol x))
		    (vector->list (vector-ref vec 0))))
	vec)
      #f))

(define (db:numrows? assoc)
  (vector-length assoc))

(define (db:fetch-row assoc row)
  (vector-ref row assoc))

(define (db:get assoc row key)
  (let ((key-loc
	 (list-index
	  (lambda (x) (eq? x key))
	  (vector-ref assoc 0))))
    (cond
     (key-loc
      (vector-ref (vector-ref assoc (1+ row)) key-loc))
     (else
      #f))))