;     Guile Scheme Data Serializer, part of Guile Web
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

;;; This is v2, a complete rewrite of the old serializer. This one
;;; actually works.

(define-module (web serialize)
  #:export (serialize unserialize add-unserializer
	    %serialize @serialize object->sobject)
  #:use-module (oop goops internal))

;;; Use a vector instead of a hash table so that it can be printed out
;;; when stored to disk with Guile 1.7/1.8
(define unserial-table (make-vector 31 (list)))

(define object->sobject object->string)

(define-class <unserializing> ()
  (object #:init-keyword #:object))

(define-class <userializing1> ()
  (object))

(define-method (change-class (obj <unserializing>) (new <class>))
  (format #t "change-class: ~A:~A " obj (class-of obj))
  (let ((object (slot-ref obj 'object)))
    ;; We shallow-clone the instance so that its class doesn't change
    (%modify-instance obj (shallow-clone object))))

;;; Define this for your object, for compound data types use
;;; @serialize to serialize individual members of the
;;; object. %serialize takes two arguments: object and
;;; serial-table. @serialize takes two arguments: object and
;;; serial-table. Pass serial-table to @serialize.
(define-generic %serialize)

(define-method (%serialize (item <top>) serial-table)
  ;; This is the default version of serialize. An attempt to
  ;; unserialize data serialized with this function will result in the
  ;; variable having the value of #f
  (object->sobject (cons '<top> #f)))

(define (add-unserializer type de-serialize)
  (if (procedure? de-serialize)
    (hashv-create-handle! unserial-table type de-serialize)
    (error "Unserializer is not a procedure" de-serialize)))

(define (%unserialize serialized-object serial-table)
  (cond ((string? serialized-object)
	 (let ((s (read (open-input-string serialized-object))))
	   (if (pair? s)
	       (let* ((type (car s))
		      (value (cdr s))
		      (proc (hashv-ref unserial-table type)))
		 (if proc
		     (proc value serial-table)
		     #f ))
	       #f)))
	(else
	 serialized-object)))
		      
;;; @serialize serializes an object by seeing if it is in the
;;; serial-table; if it isn't it serializes the object using
;;; %serialize, stores that into the serial-table, and then returns
;;; the key in the serial-table that the serialized object is at
(define (@serialize object serial-table)
  (let ((serialized?
	 (lambda (x)
	   (hashv-ref serial-table (object-address x)))))
    (cond
     ((serialized? object) (object-address object))
     (else
      ;; We put a stub in the hash table to prevent cyclical recursion
      ;; when one object points to another object that points back to
      ;; the first object
      (hashv-set! serial-table
		  (object-address object)
		  #t)
      (hashv-set! serial-table
		  (object-address object)
		  (%serialize object serial-table))))
    (object-address object)))

;;; High Level Serialization
(define (serialize . objects)
  (let* ((serial-table (make-vector 31 (list)))
	(serialized?
	 (lambda (x)
	   (hashv-ref serial-table (object-address x)))))
    (cons serial-table
	  (map (lambda (x) (@serialize x serial-table)) objects))))

;;; @unserialize returns the unserialized form of the address passed
;;; to it
(define (@unserialize object-key serial-table)
  (let ((obj (make <unserializing>
	       #:object (hashv-ref serial-table object-key)))
	(being-unserialized?
	 ;; this is true is an object is current being unserialized to
	 ;; prevent cyclical recursion
	 (lambda ()
	   (let ((obj (hashv-ref serial-table object-key)))
	     (if (eq? (class-of obj) <unserializing>)
		 #t #f)))))
    ;; Only unserialize an object once
    (cond ((being-unserialized?) obj)
	  (else
; 	   (format #t "unserializing object: ~A~%"
; 		   (slot-ref
; 		    (hashv-set! serial-table
; 				object-key
; 				obj)
; 		    'object))
	   (hashv-set! serial-table
		       object-key
		       (begin
			 (slot-set! obj 'object
				    (%unserialize
				     (slot-ref obj 'object)
				     serial-table))
			 (slot-ref obj 'object)))))))

;;; High Level Unserialization
(define (unserialize serialized-data)
  (let ((serial-table (car serialized-data))
	(data (cdr serialized-data)))
    (let ((unserialized-list
	   (map (lambda (x) (@unserialize x serial-table)) data)))
      (class-redefinition <unserializing> <userializing1>)
      (set! <unserializing> (class () (object #:init-keyword #:object)))
      (add-method!
       change-class
       (method ((obj <unserializing>) (new <class>))
	       (format #t "change-class: ~A:~A " obj (class-of obj))
	       (let ((object (slot-ref obj 'object)))
		 ;; We shallow-clone the instance so that its class
		 ;; doesn't change
		 (%modify-instance obj (shallow-clone object)))))
      unserialized-list)))

;;; serializers and unserializers for built in types that I care
;;; about...(incomplete)

;;; Serialization of simple types
(define-method (%serialize (item <number>) serial-table)
  (object->sobject (cons '<number> item)))

(define-method (%serialize (item <char>) serial-table)
  (object->sobject (cons '<char> item)))

(define-method (%serialize (item <keyword>) serial-table)
  (object->sobject (cons '<keyword> item)))

(define-method (%serialize (item <string>) serial-table)
  (object->sobject (cons '<string> item)))

(define-method (%serialize (item <symbol>) serial-table)
  (object->sobject (cons '<symbol> item)))

(define-method (%serialize (item <boolean>) serial-table)
  (object->sobject (cons '<boolean> item)))

;;; More complicated types
(define-method (%serialize (item <vector>) serial-table)
  (object->sobject (cons '<vector>
			(map (lambda (x) (@serialize x serial-table))
				(vector->list item)))))

;;; Performance hack
(define (serialize-list item serial-table)
  (object->sobject (cons '<list>
			(map (lambda (x) (@serialize x serial-table)) item))))

(define-method (%serialize (item <list>) serial-table)
  (serialize-list item serial-table))

(define-method (%serialize (item <pair>) serial-table)
  (if (list? item) ; lists are also pairs...
      ;; I do this for efficiency; the output of (serialize-list) is
      ;; /much/ smaller than that of serializing the list as a pair
      (serialize-list item serial-table)
      (object->sobject (cons '<pair>
			    (cons 
			     (@serialize (car item) serial-table)
			     (@serialize (cdr item) serial-table))))))

;;; add unserializers the easy way for basic types
(define (ret-arg x y) x) ; save space
(for-each (lambda (x) (add-unserializer x ret-arg))
	  '(<number> <char> <keyword> <string> <symbol> <top> <boolean>))

(add-unserializer '<list>
		  (lambda (serial-list table)
		    (map (lambda (x) (@unserialize x table)) serial-list)))

(add-unserializer '<vector>
		  (lambda (serial-vector table)
		    (list->vector (map (lambda (x) (@unserialize x table))
				       serial-vector))))

(add-unserializer '<pair>
		 (lambda (serial-pair table)
		   (cons (@unserialize (car serial-pair) table)
			 (@unserialize (cdr serial-pair) table))))

;;; GOOPS Objects
(define-method (%serialize (item <object>) serial-table)
  (let ((class (class-of item)))
    (object->sobject
     (cons '<object>
	   (cons (class-name class)
		 (map (lambda (x)
			(cons (car x) (@serialize
				       (slot-ref item (car x))
				       serial-table)))
		      (class-slots class)))))))
  
(add-unserializer '<object>
		  (lambda (object table)
		    ;; I know eval is evil, this is the only way (that
		    ;; I can see). I really need to support
		    ;; serialization of modules so that an object can
		    ;; be reconstructed in the environment it was
		    ;; created it.
		    (let*
			((class (eval (car object)
				      (interaction-environment)))
			 (new-object (make class)))
		      (for-each (lambda (x)
				  (slot-set! new-object (car x)
					     (@unserialize (cdr x) table)))
				(cdr object))
		      new-object)))

;;; Procedures (this probably won't work too well)
(define-method (%serialize (item <procedure>) serial-table)
  (object->sobject
   (cons '<procedure>
	(procedure-source item))))

(add-unserializer '<procedure>
		  (lambda (proc table)
		    ;; I know...
		    (eval proc (interaction-environment))))