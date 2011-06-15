;     Guile Scheme xhtml generator, part of Guile Web
;     Copyright (C) 2002,2004,2005 Clinton Ebadi

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

(define-module (web xhtml)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:use-syntax (ice-9 syncase)
  #:export (empty-tag non-empty-tag <html-document>
	    xhtml:output-port xhtml:common-attributes
	    xhtml:print xhtml:DOCUMENT xhtml:real-print xhtml:dtd
	    xhtml:tags xhtml:tag-tree->shtml))

(define-class <html-document> ()
  (output-port #:init-keyword #:port
	       #:init-form (current-output-port)
	       #:getter xhtml:output-port)
  (attributes #:init-keyword #:attributes
	      #:init-value '((id "") (class "") (style "") (title ""))
	      #:getter xhtml:common-attributes))

(define-class <xhtml-tag> ()
  (tree #:init-keyword #:tag-tree
	    #:accessor tag-tree)
  (name #:init-keyword #:name
	    #:accessor tag-name)
  (atts #:init-keyword #:atts
	    #:accessor tag-atts
	    #:allocation #:instance)
  (start-or-close #:init-keyword #:start-or-close
		  #:getter tag-start-or-close))

(define-class <xhtml-tag-top> (<xhtml-tag>))
(define-class <xhtml-tag-empty> (<xhtml-tag>))
(define-class <xhtml-tag-list> (<xhtml-tag>))

(define-generic xhtml-tag?)

(define-method (xhtml-tag? (tag <top>)) #f)
(define-method (xhtml-tag? (tag <xhtml-tag>)) #t)

(define xhtml:DOCUMENT (make-fluid))
(fluid-set! xhtml:DOCUMENT (make <html-document>))

(define-syntax xhtml:print
  (lambda (exp)
    (syntax-case exp ()
      ((_ exp1 exps ...)
       (syntax
	(if (eq? (class-of exp1) <html-document>)
	    (with-fluids ((xhtml:DOCUMENT
			   exp1))
			 (xhtml:real-print exps ...))
	    (xhtml:real-print exp1 exps ...)))))))

(define-generic xhtml:real-print)

(define-method (xhtml:real-print . elements)
  (letrec ((oport (xhtml:output-port (fluid-ref xhtml:DOCUMENT))))
    (for-each (lambda (x) (display x oport)) elements)))

(define-method (display (tag <xhtml-tag-empty>) (port <port>))
  (format port "<~A~{~{ ~A=~S~}~} />"
	  (tag-name tag)
	  (tag-atts tag)))

(define-method (display (tag <xhtml-tag>) (port <port>))
  (case (tag-start-or-close tag)
    (('start)
     (format port "<~A~{~{ ~A=~S~}~}>"
	     (tag-name tag) (tag-atts tag)))
    (('end) (format port "</~A>" (tag-name tag)))
    (else ; normal non-empty tag
     (format port "<~A~{~{ ~A=~S~}~}>"
	     (tag-name tag) (tag-atts tag))
     (for-each (lambda (x) (display x port)) (tag-tree tag))
     (format port "</~A>" (tag-name tag)))))

(define-method (display (tag <xhtml-tag-top>) (port <port>))
  (format port "<?xml version=\"1.0\" ~{~{ ~A=~S~}~} />~%<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\n\t\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">~%" (tag-atts tag))
  (for-each (lambda (x) (display x port)) (tag-tree tag)))

(define-method (display (tag <xhtml-tag-list>) (port <port>))
  (for-each (lambda (x) (display x port))(tag-tree tag)))


(define (non-empty-attributes attributes)
  "returns all non-empty (i.e. not string-null?) attributes from attributes"
  (let loop ((non-empty (list))
	     (rest attributes))
    (cond ((null? rest) non-empty)
	  (else
	   (if (string-null? (cadar rest))
	       (loop non-empty (cdr rest))
	       (loop (cons (car rest) non-empty)
		     (cdr rest)))))))

(define (merge-atts tag-defaults tag-values)
  "returns a new attlist using system for default values"
  (let ((system-copy (append! (copy-tree (xhtml:common-attributes
					  (fluid-ref xhtml:DOCUMENT)))
			      (copy-tree tag-defaults))))
    (for-each (lambda (x) (set! system-copy
			   ;; (list (cdr x)) is used so that the alist
			   ;; entry is a proper list so that format's
			   ;; iteration can use it
			   (assq-set! system-copy (car x) (list (cdr x)))))
	      tag-values)
    system-copy))

(define (empty-tag name attributes)
  (lambda* (#:key (atts '()))
	   (let ((new-attlist
		  (non-empty-attributes
		   (merge-atts attributes atts))))
	     (make <xhtml-tag-empty> #:name name #:atts new-attlist))))

(define (non-empty-tag-start name attributes)
  (lambda* (#:key (atts '()))
	   (let ((new-attlist (non-empty-attributes
			       (merge-atts attributes atts))))
	     (make <xhtml-tag>
	       #:name name #:atts new-attlist
	       #:start-or-close 'start))))

(define (non-empty-tag-end name)
  (lambda ()
    (make <xhtml-tag> #:name name #:start-or-close close)))

(define (non-empty-tag name attributes)
  (lambda* (#:key (atts '()) #:rest elements)
	   (let ((new-attlist (non-empty-attributes
			       (merge-atts attributes atts))))

	     ;; the #:rest list still contains all of the #:key stuff,
	     ;; we remove it here
	     (set! elements
		   (remove!
		    (let ((del-next #f))
		      (lambda (e)
			(cond
			 ((eq? e #:atts) (set! del-next #t) #t)
			 (del-next (set! del-next #f) #t)
			 (else #f))))
		    elements))

	     (make <xhtml-tag>
	       #:name name #:atts new-attlist
	       #:start-or-close #f
	       #:tag-tree elements))))

(define xhtml:dtd
  (let ((default-attributes '((encoding . "iso-8859-1"))))
    (lambda* (#:key (atts '((encoding . "iso-8859-1"))) #:rest elements)
	(let ((new-attlist
	       (non-empty-attributes
		(merge-atts default-attributes atts))))
	  (make <xhtml-tag-top>
	    #:tag-tree elements
	    #:atts new-attlist)))))

(define (xhtml:tags . tags)
  (make <xhtml-tag-list> #:tag-tree tags))

;;; HtmlPrag
(define-generic xhtml:tag-tree->shtml)
(define-generic htmlpragize)

(define-method (htmlpragize (tag <xhtml-tag>))
  (cons* (tag-name tag) (cons* '@ (tag-atts tag))
	(map htmlpragize (tag-tree tag))))

(define-method (htmlpragize (tag <xhtml-tag-top>))
  (cons* '*TOP* (map htmlpragize (tag-tree tag))))

(define-method (htmlpragize (tag <xhtml-tag-empty>))
  (list (tag-name tag) (cons* '@ (tag-atts tag))))

(define-method (htmlpragize (tag <xhtml-tag-list>))
  (map htmlpragize (tag-tree tag)))

(define-method (htmlpragize (item <top>))
  (with-output-to-string (lambda () (display item))))

(define-method (xhtml:tag-tree->shtml (tag <xhtml-tag>))
  (htmlpragize tag))

(load-from-path "web/html/html-defined.scm")
