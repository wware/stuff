;;; (www server-utils modlisp) --- Handlers for Apache mod_lisp protocol

;; Copyright (C) 2010 Thien-Thi Nguyen
;;
;; This file is part of Guile-WWW.
;;
;; Guile-WWW is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; Guile-WWW is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with Guile-WWW; see the file COPYING.  If not,
;; write to the Free Software Foundation, Inc., 51 Franklin Street,
;; Fifth Floor, Boston, MA  02110-1301  USA

;;; Commentary:

;; The (www server-utils modlisp) module
;; is fully documented in the guile-www.info file.

;;; Code:

(define-module (www server-utils modlisp)
  #:export (modlisp-hgrok
            modlisp-ish)
  #:use-module ((ice-9 rdelim) #:select (read-line)))

(define stashed (make-object-property))

(define (read-headers port)
  (or (stashed port)
      (let loop ((acc '()))
        (let ((k (read-line port)))
          (if (string=? "end" k)
              (set! (stashed port)
                    ;; rv
                    (reverse! acc))
              (loop (acons k (read-line port) acc)))))))

(define (read-first-line port)
  (set! (stashed port) #f)
  (let* ((headers (read-headers port))
         (rv (map (lambda (x)
                    (assoc-ref headers x))
                  '("method" "url" "server-protocol"))))
    (set-car! rv (string->symbol (car rv)))
    rv))

(define modlisp-hgrok (vector read-first-line
                              read-headers
                              read-headers))

(define LF "\n")

(define modlisp-ish (vector "Status\n~A ~A\n"
                            LF LF
                            "end\n"))

;;; modlisp.scm ends here
