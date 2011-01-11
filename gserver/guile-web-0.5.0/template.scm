;     Guile Scheme Web Templates, part of Guile Web
;     Copyright (C) 2003,2004 Clinton Ebadi

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

(define-module (web template)
  #:use-module (ice-9 rdelim)
  #:export (template:parse template:set-error-handler))


(define (template:set-error-handler handler)
  (let ((old template:error))
    (set! template:error handler)
    old))

(define (template:error fmt . args)
  (scm-error 'web:template:error #f fmt args #f))

(define (handle-command command-table command)
  (let ((command-func (hash-ref command-table (car command))))
    (cond ((procedure? command-func)
	   (catch #t
		  (lambda ()
		    (apply command-func (cdr command)))
		  (lambda args (template:error
			   "~S" args))))
	  (else
	   (template:error "No Command Named ~A!" (car command))))))

(define (parse-template-call command-table port)
  (let parse-loop
      ((call
	(let build-command ((command (list)))
	  (let ((str (read-delimited "]" port 'split))
		(call-list->string
		 (lambda (command-list)
		   (apply string-append (reverse! command-list)))))
	    (cond ((eof-object? (cdr str))
		   (template:error "End of file in template command block"))
		  ((eof-object? (peek-char port))
		   (call-list->string (cons (car str) command)))
		  ((char=? (peek-char port) #\])
		   (read-char port)
		   (build-command (cons* "]" (car str) command)))
		  (else
		   (call-list->string (cons (car str) command))))))))
    (let ((command-sexp-port (open-input-string call)))
      (let build-command ((command-list (list)))
	(let ((current-token (read command-sexp-port)))
	  (cond ((eof-object? current-token)
		 (handle-command command-table (reverse! command-list)))
		(else
		 (build-command (cons current-token
				      command-list)))))))))

(define (template:parse command-table port)
  (let build-page-loop ((doc (list)))
    (let ((str (read-delimited "[" port 'split)))
      (cond ((eof-object? (cdr str))
	     (reverse! (cons (if (eof-object? (car str)) "" (car str))
				    doc)))
	    ((char=? (cdr str) #\[)
	     (cond ((char=? (peek-char port) #\[)
		    (read-char port)
		    (build-page-loop (cons* "[" (car str) doc)))
		   (else
		    (build-page-loop (cons* (parse-template-call command-table
								 port)
					    (car str)
					    doc)))))
	    (else
	     ;; This shouldn't happen
	     (template:error "wtf"))))))