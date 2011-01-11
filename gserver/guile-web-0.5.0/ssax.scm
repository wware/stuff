;     Guile Scheme SSAX Module, part of Guile Web
;     Copyright (C) 2002 Clinton Ebadi

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

;;; This is a wrapper for Oleg's SSAX 
(define-module (web ssax)
  #:export (xml-token?
	    xml-token-kind xml-token-head
	    make-empty-attlist attlist-add
	    attlist-null?
	    attlist-remove-top
	    attlist->alist attlist-fold
	    SSAX:skip-internal-dtd
	    SSAX:read-markup-token
	    SSAX:read-CDATA-body
	    SSAX:read-char-ref
	    SSAX:read-attributes
	    SSAX:complete-start-tag
	    SSAX:read-external-ID
	    SSAX:read-char-data
	    SSAX:make-pi-parser 
	    SSAX:make-elem-parser 
	    SSAX:make-parser
	    SSAX:XML->SXML
	    SSAX:warn
	    parser-error))


;;; All the symbols defined in SSAX are:

;;; make-xml-token xml-token? xml-token-kind xml-token-head SSAX:warn
;;; parser-error equal_? assq-values fold-right fold SSAX:S-chars
;;; SSAX:skip-S SSAX:ncname-starting-char? SSAX:read-NCName
;;; SSAX:read-QName SSAX:Prefix-XML name-compare SSAX:skip-pi
;;; SSAX:read-pi-body-as-string SSAX:skip-internal-dtd
;;; make-empty-attlist attlist-add attlist-null? attlist-remove-top
;;; attlist->alist SSAX:resolve-name SSAX:scan-Misc 

;;; These appear to be the public procedures:

;;; SSAX:read-markup-token SSAX:read-CDATA-body SSAX:read-char-ref
;;; SSAX:handle-parsed-entity SSAX:read-attributes
;;; SSAX:uri-string->symbol SSAX:complete-start-tag
;;; SSAX:complete-start-tag SSAX:read-external-ID SSAX:read-char-data
;;; SSAX:assert-token SSAX:make-pi-parser SSAX:make-elem-parser
;;; SSAX:make-parser SSAX:XML->SXML 


;;; User should be able to override these
(define (SSAX:warn port msg . other-msg)
   (apply cerr (cons* "\nWarning: " msg other-msg)))

(define (parser-error port msg . specializing-msgs)
   (apply error (cons msg specializing-msgs)))

(load-from-path "web/internal/pp.scm")
(load-from-path "web/internal/common.scm")
(load-from-path "web/internal/myenv.scm")
(load-from-path "web/internal/parse-error.scm")
(load-from-path "web/internal/util.scm")
(load-from-path "web/internal/input-parse.scm")
(load-from-path "web/internal/look-for-str.scm")
(load-from-path "web/internal/SSAX/ssax-code.scm")
