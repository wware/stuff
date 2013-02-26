(use-modules (ice-9 format))

(define-macro (define-tag! tagname atts empty?)
  (format #t "@item Tag: @code{~A}~%~/@itemize @minus~%~/@item Atts:~%~/~/@itemize~{~{~%~/~/@item Name: @code{~A} Default Value: @code{~S}~}~}~%~/~/@end itemize~%~/~A~%~/@end itemize~%"
	  tagname
	  atts
	  (if empty? "@item Empty Tag" "")))

(format #t "@itemize @bullet~%")
(load "html-defines.scm")
(format #t "@end itemize~%")
