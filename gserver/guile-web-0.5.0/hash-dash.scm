;     Guile Scheme "Hash-Dash" extension, part of Guile Web
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

(define-module (web hash-dash)
  #:use-module (ice-9 rdelim))
;;; This doesn't export anything, it only extends the system with the
;;; #- -# syntax.

;;; To use, do this:
;;; #-
;;; [LARGE BLOCK OF TEXT]
;;; -#
;;; It then returns "[LARGE BLOCK OF TEXT]" in place

(define (hash-dash char port)
  (let build-string ((string-list (list)))
    (let ((current-string
	   (read-delimited "-" port 'split)))
      (cond ((eof-object? (cdr current-string))
	     (error "End of File in #- -# block"))
	    ((char=? (peek-char port) #\#)
	     (read-char port)
	     (apply string-append (reverse!
				   (cons
				    (car current-string)
				    string-list))))
	    (else
	     (build-string (cons* (string (cdr current-string))
				  (car current-string)
				  string-list)))))))

(read-hash-extend #\- hash-dash)