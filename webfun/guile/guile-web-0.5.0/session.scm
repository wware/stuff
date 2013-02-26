;     Guile Scheme Session Management, part of Guile Web
;     Copyright (C) 2002,2003,2004 Clinton Ebadi

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

(define-module (web session)
  #:export (session:start session:end session:destroy
	    session:register-var session:alter-var!
	    session:var-exists? session:unregister-var
	    session:set-save-path! session:set-cookie-name!
	    session:get-value)
  #:use-module (web serialize)
  #:use-module (oop goops)
  #:use-module (srfi srfi-17) ; generalized set
  )

;; Load session:_* functions (the C interface to libcgi's sessions)
(load-extension "libgwebsession" "scgi_session_init")

;; Use a vector of alists instead of a "real" hash table so that it
;; can be easily printed out
(define *session-hash-table* (make-vector 31 (list)))

;;; Define an setter+getter for *session-hash-table* that locks a
;;; mutex before doing anything to it and then convert everything to
;;; use it


(define (s:s->o string)
  (read (open-input-string string)))

(define (session:start)
  (cond
   ((session:_start)
    (if (session:_var-exists? 'session-table)
	(set! *session-hash-table*
	      (unserialize (s:s->o (session:_get-value 'session-table))))
	(session:_register-var 'session-table
			       (object->string
				(serialize *session-hash-table*))))
    #t)
   (else
    #f)))

(define (session:save)
  (session:_alter-var 'session-table
		      (object->string (serialize *session-hash-table*))))

(begin-deprecated
 (define (session:end)
   (issue-deprecation-warning
    "`session:end' is deprecated, use `session:save' instead.")
   (session:save)))

(define (session:register-var name value)
  (hashv-set! *session-hash-table* name value))

(define (session:alter-var! name value)
  (hashv-set! *session-hash-table* name value))

(define (session:get-value name)
  (hashv-ref *session-hash-table* name))

(define (session:var-exists? name)
  (if (hashv-get-handle *session-hash-table* name)
      #t #f))
