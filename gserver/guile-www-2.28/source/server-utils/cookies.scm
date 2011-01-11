;;; (www server-utils cookies) --- Handling bits of client-side state

;; Copyright (C) 2009, 2010 Thien-Thi Nguyen
;; Copyright (C) 2006 Free Software Foundation, Inc.
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

(define-module (www server-utils cookies)
  #:export (rfc2109-set-cookie-string
            simple-parse-cookies
            rfc2965-set-cookie2-tree
            rfc2965-parse-cookie-header-value
            reach)
  #:use-module ((srfi srfi-13) #:select ((substring/shared . subs)))
  #:use-module (ice-9 regex)
  #:use-module (ice-9 optargs))

;;;---------------------------------------------------------------------------
;;; RFC2109

;; Return a string suitable for inclusion into an HTTP response header
;; as a cookie with @var{name} and @var{value}.  Both args may be strings,
;; symbols or keywords.  Also, recognize and format appropriately the
;; optional keyword parameters @code{#:path}, @code{#:domain},
;; @code{#:expires} (strings); and @code{#:secure} (boolean).
;;
;;-sig: (name value [keyword arg...])
;;
(define* (rfc2109-set-cookie-string name value #:key (path #f)
                                    (domain #f) (expires #f) (secure #f))
  (define (fs s . args)
    (apply simple-format #f s args))
  (fs "Set-Cookie: ~A=~A~A~A~A~A"
      (if (keyword? name) (keyword->symbol name) name)
      (if (keyword? value) (keyword->symbol value) value)
      (if path (fs "; path=~A" path) "")
      (if domain (fs "; domain=~A" domain) "")
      (if expires (fs "; expires=~A" expires) "")
      (if secure "; secure" "")))

;;;---------------------------------------------------------------------------
;;; simple parsing

(define +pair-exp+ (make-regexp "(,[ \t]*)*([^=]+)=([^,]+)"))

;; Parse @var{string} for cookie-like fragments using the simple regexp:
;; @example
;; (,[ \t]*)*([^=]+)=([^,]+)
;; @end example
;;
;; Return a list of elements @code{(@var{name} . @var{value})},
;; where both @var{name} and @var{value} are strings.  For example:
;;
;; @example
;; (simple-parse-cookies "abc=def; z=z, ans=\"42\", abc=xyz")
;; @result{} (("abc" . "def; z=z") ("ans" . "\"42\"") ("abc" . "xyz"))
;; @end example
;;
(define (simple-parse-cookies string)
  (define (kv<-m m)
    (cons (match:substring m 2)
          (match:substring m 3)))
  (map kv<-m (list-matches +pair-exp+ string)))

;;;---------------------------------------------------------------------------
;;; RFC2965

(define *attribute-names*
  (map (lambda (kw)
         (cons kw (symbol->string (keyword->symbol kw))))
       '(#:Comment
         #:CommentURL
         #:Discard
         #:Domain
         #:Max-Age
         #:Path
         #:Port
         #:Secure
         ;; Automatically appended; cannot be user-specified.
         ;;- #:Version
         )))

;; Compute a list suitable for inclusion in an HTTP response header,
;; composed by formatting @var{cookie-specs}, each a list of the form
;; @code{(@var{name} @var{value} @var{a1} @var{v1@dots{}})}.  Each
;; @var{name} may be a string, symbol or keyword.  Each @var{value} may
;; be a string or symbol.  Each @var{a} must be a keyword, precisely one
;; of:
;;
;; @example
;; #:Comment  #:CommentURL  #:Discard  #:Domain
;; #:Max-Age  #:Path  #:Port  #:Secure
;; @end example
;;
;; The #:Version attribute is automatically included as the last one;
;; it cannot be specified (or de-specified).
;;
;; Possible values for @var{v} depend on @var{a}.  If @var{a} is
;; @code{#:Discard} or @code{#:Secure}, then there is no @var{v} (it must
;; be omitted).  If @var{a} is @code{#:Port}, then @var{v} must be either
;; a number; a list of numbers, for instance @code{(8001 8002 8003)}; or
;; omitted entirely.  If @var{a} is @code{#:Max-Age}, then @var{v} must
;; be a number.  For all other @var{a}, @var{v} can be a string or symbol.
;;
;; If @var{M} is @code{#f}, return a list.  The @sc{car} of the list is
;; the keyword @code{#:Set-Cookie2}, and the @sc{cdr} is a tree of
;; strings.  Otherwise @var{M} should be a @code{mouthpiece}
;; (@pxref{answer}) in which case it is applied with the
;; @code{#:add-header} command to the list.
;;
(define (rfc2965-set-cookie2-tree M . cookie-specs)

  (define (csep q proc init)
    (if (pair? init)
        (let* ((ls (map proc init))
               (rv (list (car ls)))
               (tp rv))
          (define (ok x)
            (set! tp (append! (last-pair tp) x)))
          (and q (set! rv (cons q rv)))
          (let loop ((ls (cdr ls)))
            (cond ((null? ls)
                   (and q (ok (list q))))
                  (else
                   (ok (list "," (car ls)))
                   (loop (cdr ls)))))
          rv)
        init))

  (define (pair<- x y)
    (list x "=" (if (pair? y)
                    y
                    (simple-format #f "\"~A\"" y))))

  (define (tree<- name value . more)
    (let* ((rv (pair<-
                (cond ((keyword? name) (symbol->string (keyword->symbol name)))
                      ((string? name) name)
                      ((symbol? name) (symbol->string name))
                      (else (error "bad name:" name)))
                value))
           (tp rv))
      (define (ok x)
        (set! tp (append! (last-pair tp) (cons ";" x))))
      (let loop ((ls more))
        (or (null? ls)
            (let* ((attr (car ls))
                   (aname (or (assq-ref *attribute-names* attr)
                              (error "invalid attribute:" attr))))
              (define (okv value)
                (ok (pair<- aname value)))
              (case attr
                ((#:Discard #:Secure)
                 (ok (list aname))
                 (loop (cdr ls)))
                ((#:Max-Age)
                 (ok (list aname "=" (number->string (cadr ls))))
                 (loop (cddr ls)))
                ((#:Port)
                 (let* ((ports (and (not (null? (cdr ls)))
                                    (cadr ls)))
                        (none? (or (not ports) (keyword? ports))))
                   (if none?
                       (ok (list aname))
                       (okv (csep "\"" number->string
                                  (if (pair? ports)
                                      ports
                                      (list ports)))))
                   (loop ((if none? cdr cddr) ls))))
                (else
                 (okv (cadr ls))
                 (loop (cddr ls)))))))
      (ok (list "Version=1"))
      rv))

  (let ((hname #:Set-Cookie2)
        (hval (csep #f (lambda (c)
                         (apply tree<- c))
                    cookie-specs)))
    (if M
        (M #:add-header hname hval)
        (cons hname hval))))

;; Parse the @code{Cookie} HTTP response header string @var{s}.
;; Return a list of the form @code{(@var{vers} @var{n}
;; [@var{cookie-spec}@dots{}])}, where @var{vers} is the version number
;; of the cookie specification, 0 (zero) for RFC2109 compliance and 1
;; (one) for RFC2965 compliance; and @var{n} is the number of
;; cookie-specs the @sc{cdr} of the form.
;;
;; Each @var{cookie-spec} has the form: @code{(@var{name} @var{value} @var{a1}
;; @var{v1}@dots{})}.
;; @var{name}, @var{value} are strings.  Each @var{a} is a keyword,
;; one of @code{#:Path}, @code{#:Domain} or @code{#:Port}.  Each @var{v}
;; is a string, except for that associated with @code{#:Port}, which
;; is can be either a single number or a list of numbers.
;;
;; Optional @var{flags} configure the parsing and/or return value.
;;
;; @table @code
;; @item #:keep-attribute-dollarsign-prefix
;; Prevent conversion of, for example, @code{#:$Port} to @code{#:Port}.
;;
;; @item #:strict-comma-separator
;; Disable support for older clients that use a semicolon to separate
;; cookies instead of a comma.  Normally, parsing copes (heuristically)
;; with this by reparsing an unrecognized attribute as the beginning of a
;; new cookie.  With this flag, an unrecognized attribute signals an error.
;;
;; @item #:canonicalize-NAME-as-keyword
;; Convert the @var{name} in each cookie-spec into a keyword whose
;; first character and characters following a hyphen are upcased.
;; For example, "session-id-no" would become @code{#:Session-Id-No}.
;; @end table
;;
;; Parsing may signal an error and display an error message in the form:
;; ``@var{situation} while @var{context}'', where @var{situation} is one of
;; ``unexpected end'', ``missing equal-sign'', ``bad attribute'', or
;; ``missing semicolon''; and @var{context} is one of: ``reading string'',
;; ``reading token'', ``reading pair'', ``reading one cookie'' or
;; ``parsing''.  The error message also displays string @var{s} on a line
;; by itself and on the next line a caret by itself indented to be at
;; (or near) the site of the error.
;;
(define (rfc2965-parse-cookie-header-value s . flags)
  (let* ((as-is? (memq #:keep-attribute-dollarsign-prefix flags))
         (strict? (memq #:strict-comma-separator flags))
         (canon? (memq #:canonicalize-NAME-as-keyword flags))
         (len (string-length s))
         (rv (list 0))
         (tp rv)
         (context '())
         (pos 0))

    (define (context! x)
      (set! context (cons x context)))

    (define (context- v)
      (set! context (cdr context))
      v)

    (define (err! blurb)
      (error (simple-format #f "~A while ~A\n~A\n~A^" blurb (car context)
                            s (make-string pos #\space))))

    (define (sw!)                       ; skip whitespace
      (and (char=? #\space (string-ref s pos))
           (begin (set! pos (1+ pos)) (sw!))))

    (define (fc!)                       ; forward char
      (set! pos (1+ pos))
      (or (< pos len)
          (err! "unexpected end")))

    (define (fc!/nocheck)
      (set! pos (1+ pos)))

    (define (read-string)
      (context! "reading string")
      (fc!)
      (let ((start pos))
        (let loop ((c (string-ref s pos)))
          (if (char=? #\" c)
              (let ((rv (subs s start pos)))
                (fc!/nocheck)
                (context- rv))
              (loop (string-ref s (begin (fc!) pos)))))))

    (define (->kw x)                    ; e.g., "$mAx-aGe" => #:$Max-Age
      (symbol->keyword
       (string->symbol
        (let* ((new (string-downcase x))
               (first-letter (if (char=? #\$ (string-ref new 0))
                                 1 0)))
          (define (up! i)
            (string-set! new i (char-upcase (string-ref new i))))
          (up! first-letter)
          (let loop ((i (1+ first-letter)))
            (cond ((string-index new #\- i)
                   => (lambda (hyphen)
                        (up! (1+ hyphen))
                        (loop (+ 2 hyphen))))))
          new))))

    (define (read-token munge)
      (context! "reading token")
      (sw!)
      (let ((return (or munge identity))
            (start pos)
            (last-pos (1- len)))
        (let loop ((c (string-ref s pos)))
          (cond ((= pos last-pos)
                 (let ((rv (subs s start len)))
                   (fc!/nocheck)
                   (context- (return rv))))
                ((memq c '(#\= #\; #\, #\space #\ht))
                 (let ((rv (subs s start pos)))
                   (context- (return rv))))
                (else
                 (loop (string-ref s (begin (fc!) pos))))))))

    (define (read-pair one-munge)       ; "ONE=TWO" => (ONE TWO)
      (context! "reading pair")
      (let ((one (read-token one-munge))
            (expected-sep (if (begin (sw!) (char=? #\= (string-ref s pos)))
                              (fc!)
                              (err! "missing equal-sign")))
            (two (let ((c (begin (sw!) (string-ref s pos))))
                   (case c
                     ((#\") (read-string))
                     (else (read-token #f))))))
        (context- (list one two))))

    (define (portlist s)
      (let loop ((b 0) (acc '()))
        (let sw ()
          (and (char=? #\space (string-ref s b))
               (begin (set! b (1+ b)) (sw))))
        (cond ((string-index s #\, b)
               => (lambda (e)
                    (loop (1+ e) (cons (string->number (subs s b e)) acc))))
              (else
               (let ((ls (reverse! (cons (string->number (subs s b)) acc))))
                 (if (= 1 (length ls))
                     (car ls)
                     ls))))))

    (define (read-one-cookie)
      (context! "reading one cookie")
      (let* ((first-pair (read-pair #f))
             (rv first-pair)
             (tp rv))
        (define (another!)
          (and (< pos len)
               (char=? #\; (string-ref s pos))
               (fc!)
               ;; slack related to allowing semicolon to separate cookies:
               ;; reset position if not a recognized attribute (overshot).
               (let ((opos pos)
                     (p (read-pair ->kw)))
                 (and (< pos len) (sw!))
                 (cond ((assq-ref '((#:$Path   . #:Path)
                                    (#:$Domain . #:Domain)
                                    (#:$Port   . #:Port))
                                  (car p))
                        => (lambda (recognized)
                             (if as-is?
                                 p
                                 (cons recognized
                                       (let ((rest (cdr p)))
                                         (if (eq? #:Port recognized)
                                             (list (portlist (car rest)))
                                             rest))))))
                       (else
                        (set! pos opos)
                        (if strict?
                            (err! "bad attribute")
                            #f))))))
        (let loop ((more (another!)))
          (cond (more
                 (set! tp (append! (last-pair tp) more))
                 (loop (another!)))))
        (context- rv)))

    ;; do it!
    (context! "parsing")
    (let ((vers (let ((start pos)
                      (p (read-pair ->kw)))
                  (cond ((eq? #:$Version (car p))
                         (string->number (cadr p)))
                        (else
                         ;; if not found, reset position
                         (set! pos start)
                         0)))))
      (define (more!)
        (and (< pos len)
             (begin
               (and (char=? #\, (string-ref s pos))
                    (fc!))
               (read-one-cookie))))
      (sw!)
      (or (char=? #\; (string-ref s pos))
          (err! "missing semicolon"))
      (fc!)
      (let loop ((cspec (more!)))
        (cond (cspec
               (set-car! rv (1+ (car rv)))
               (and canon? (set-car! cspec (->kw (car cspec))))
               (set! tp (append! (last-pair tp) (list cspec)))
               (loop (more!)))))
      (context- (cons vers rv)))))

;; Return the @dfn{reach} (a string) of host name @var{h}.
;; Quoting from RFC2965 section 1 (Terminology):
;;
;; @format
;; The reach R of a host name H is defined as follows:
;; If
;;   - H is the host domain name of a host; and,
;;   - H has the form A.B; and
;;   - A has no embedded (that is, interior) dots; and
;;   - B has at least one embedded dot, or B is the string "local".
;; then the reach of H is .B.
;; Otherwise, the reach of H is H.
;; @end format
;;
;; Note that comparison with "local" uses @code{string=?},
;; i.e., case-sensitively.
;;
(define (reach h)
  (define (dot s) (string-index s #\.))
  (or (and=> (dot h)
             (lambda (pos)
               (and (not (dot (subs h 0 pos)))
                    (let ((b (subs h (1+ pos))))
                      (and (or (string=? "local" b)
                               (dot b))
                           (subs h pos))))))
      h))

;;; (www server-utils cookies) ends here
