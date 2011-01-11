;     Guile Scheme xhtml generator, part of Guile Web
;     Copyright (C) 2002,2004 Clinton Ebadi

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

;;; This file generates the defines for all the tags in (web html) at
;;; compile time so that the module can load a lot faster

(defmacro define-tag! (name attributes empty?)
  (let ((tag-symbol (string->symbol
		     (string-append
		      "xhtml:"
		      (symbol->string name)))))
    (let ((exp-macro (list)))
      (set! exp-macro
	    (cons*
	     `(module-export! (current-module) (list ',tag-symbol))
	     exp-macro))
      (set! exp-macro
	    (cons* `(define ,tag-symbol
		      ,(if empty?
			   `(empty-tag     ',name ',attributes)
			   `(non-empty-tag ',name ',attributes)))
		   exp-macro))
      (cond ((not empty?)
	     (begin
	       (set! exp-macro
		     (cons* `(define
			       ,(symbol-append tag-symbol '-end)
			       (non-empty-tag-end ',name))
			    exp-macro))
	       (set! exp-macro
		     (cons* `(module-export!
			      (current-module)
			      (list ',(symbol-append tag-symbol '-end)))
			    exp-macro))
	       (set! exp-macro
		     (cons* `(define ,(symbol-append tag-symbol '-start)
						     (non-empty-tag-start
						      ',name
						      ',attributes))
			    exp-macro))
	       (set! exp-macro
		     (cons* `(module-export! (current-module)
					     (list
					      ',(symbol-append tag-symbol
							       '-start)))
			    exp-macro)))))
      (write (cons 'begin (reverse! exp-macro))))))

(define (main)
  (load "html-defines.scm"))

(main)