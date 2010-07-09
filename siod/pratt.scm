;; -*-mode:lisp-*-
;;
;; A simple Pratt-Parser for SIOD: 2-FEB-90, George Carrette, GJC@PARADIGM.COM
;; Siod may be obtained by anonymous FTP to FTP.STD.COM
;; Look in the directory pub/gjc
;;
;;                   COPYRIGHT (c) 1990 BY                       
;;     PARADIGM ASSOCIATES INCORPORATED, CAMBRIDGE, MASSACHUSETTS.
;;         See the source file SLIB.C for more information. 
;;
;;
;; Based on a theory of parsing presented in:                       
;;                                                                      
;;  Pratt, Vaughan R., ``Top Down Operator Precedence,''         
;;  ACM Symposium on Principles of Programming Languages         
;;  Boston, MA; October, 1973.                                   
;;                                                                      
;; $Id: pratt.scm,v 1.5 1996/03/01 19:39:36 gjc Exp $
;;

;; The following terms may be useful in deciphering this code:

;; NUD -- NUll left Denotation (op has nothing to its left (prefix))
;; LED -- LEft Denotation      (op has something to left (postfix or infix))

;; LBP -- Left Binding Power  (the stickiness to the left)
;; RBP -- Right Binding Power (the stickiness to the right)
;;
;;

;; Example calls
;;
;; (pl '(f #.OPEN-PAREN a #.CLOSE-PAREN = a + b / c)) => (= (f a) (+ a (/ b c)))
;;
;; (pl '(if g #.OPEN-PAREN a #.COMMA b #.CLOSE-PAREN then a > b else k * c + a * b))
;;  => (if (g a b) (> a b) (+ (* k c) (* a b)))
;;
;; Notes: 
;;
;;   This code must be used with siod.scm loaded, in siod version 2.3
;;
;;   For practical use you will want to write some code to
;;   break up input into tokens.

(define COMMA (intern ","))
(define OPEN-PAREN (intern "("))
(define CLOSE-PAREN (intern ")"))
(define SEMICOLON (intern ";"))
(define QUOTE (intern "'"))

(define (pl l)
  ;; parse a list of tokens
  (set! l (append l '($)))
  (toplevel-parse (lambda (op arg)
		    (cond ((eq? op 'peek)
			   (if l (car l) (eof-val)))
			  ((eq? op 'get)
			   (if l (pop l) (eof-val)))))))

(define (token-peek stream)
  (stream 'peek nil))

(define (token-read stream)
  (stream 'get nil))

(define (toplevel-parse stream)
  (if (eq? (eof-val) (token-peek stream))
      (token-read stream)
    (prog1 (parse -1 stream)
      (if (eq? '$ (token-peek stream)) (token-read stream)))))

(define (value-if-symbol x)
  (if (symbol? x)
      (symbol-value x)
    x))

(define (nudcall token stream)
  (if (symbol? token)
      (if (get-syntax token 'nud)
	  ((value-if-symbol (get-syntax token 'nud)) token stream)
	(if (get-syntax token 'led)
	    (error 'not-a-prefix-operator token)
	  token)
	token)
    token))

(define (ledcall token left stream)
  ((value-if-symbol (or (and (symbol? token)
			     (get-syntax token 'led))
			(error 'not-an-infix-operator token)))
   token
   left
   stream))

(define (lbp token)
  (or (and (symbol? token) (get-syntax token 'lbp))
      200))

(define (rbp token)
  (or (and (symbol? token) (get-syntax token 'rbp))
      200))

(define (parse rbp-level stream)
  (define (parse-loop translation)
    (if (< rbp-level (lbp (token-peek stream)))
	(parse-loop (ledcall (token-read stream) translation stream))
      translation))
  (parse-loop (nudcall (token-read stream) stream)))

(define (header token)
  (or (get-syntax token 'header) token))

(define (parse-prefix token stream)
  (list (header token)
	(parse (rbp token) stream)))

(define (parse-infix token left stream)
  (list (header token)
	left
	(parse (rbp token) stream)))

(define (parse-nary token left stream)
  (cons (header token) (cons left (prsnary token stream))))

(define (parse-matchfix token stream)
  (cons (header token)
	(prsmatch (or (get-syntax token 'match) token)
		  (or (get-syntax token 'comma) '#.COMMA)
		  stream)))

(define (parse-matchfix-modified token stream)
  (cons (header token)
	(prsmatch-modified (or (get-syntax token 'match) token)
			   (or (get-syntax token 'comma) '#.COMMA)
			   stream)))

(define (prsnary token stream)
  (define (loop l)
    (if (eq? token (token-peek stream))
	(begin (token-read stream)
	       (loop (cons (parse (rbp token) stream) l)))
      (reverse l)))
  (loop (list (parse (rbp token) stream))))

(define (prsmatch token comma stream)
  (cond ((eq? token (token-peek stream))
	 (token-read stream)
	 nil)
	('else
	 (define (loop l)
	   (cond ((eq? token (token-peek stream))
		  (token-read stream)
		  (reverse l))
		 ((eq? comma (token-peek stream))
		  (token-read stream)
		  (loop (cons (parse 10 stream) l)))
		 ('else
		  (error 'comma-or-match-not-found (token-read stream)))))
	 (loop (list (parse 10 stream))))))

(define (prsmatch-modified token comma stream)
  (cond ((eq? token (token-peek stream))
	 (token-read stream)
	 nil)
	('else
	 (define (loop l)
	   (cond ((eq? token (token-peek stream))
		  (token-read stream)
		  (reverse l))
		 ((eq? comma (token-peek stream))
		  (token-read stream)
		  (cond ((eq? token (token-peek stream))
			 (token-read stream)
			 (reverse l))
			('else
			 (loop (cons (parse 10 stream) l)))))
		 ('else
		  (error 'comma-or-match-not-found (token-read stream)))))
	 (loop (list (parse 10 stream))))))

(define (delim-err token stream)
  (error 'illegal-use-of-delimiter token))

(define (erb-err token left stream)
  (error 'too-many token))

(define (premterm-err token stream)
  (error 'premature-termination-of-input token))

(define *syntax-table* (cons-array 10))

(define (get-syntax token key)
  (href (or (href *syntax-table* key) #(())) token))

(define (set-syntax token key value)
  (hset (or (href *syntax-table* key)
	    (hset *syntax-table* key (cons-array 10)))
	token
	value))

(define (defsyntax-macro form)
  (list '*defsyntax (list 'quote (cdr form))))

(define defsyntax 'defsyntax-macro)

(define (*defsyntax input)
  (let ((l (cdr input)))
    (while l
      (set-syntax (car input) (car l) (cadr l))
      (set! l (cddr l)))))

(defsyntax $
  lbp -1
  nud premterm-err)

(defsyntax #.COMMA
  lbp 10
  nud delim-err)


(defsyntax #.CLOSE-PAREN
  nud delim-err
  led erb-err
  lbp 5)

(defsyntax #.OPEN-PAREN
  nud open-paren-nud
  led open-paren-led
  lbp 200)

(defsyntax #.SEMICOLON
  lbp -1
  nud delim-err)

(defsyntax {
  header begin
  comma #.SEMICOLON
  match }
  nud parse-matchfix-modified
  lbp 200)

(defsyntax }
  nud delim-err
  led erb-err
  lbp 5)

(defsyntax if
  nud if-nud
  rbp 45)

(defsyntax then
  nud delim-err
  lbp 5
  rbp 25)

(defsyntax else
  nud delim-err
  lbp 5
  rbp 25)

(defsyntax -
  nud parse-prefix
  led parse-nary
  lbp 100
  rbp 100)

(defsyntax +
  nud parse-prefix
  led parse-nary
  lbp 100
  rbp 100)

(defsyntax *
  led parse-nary
  lbp 120)

(defsyntax =
  led parse-infix
  lbp 80
  rbp 80)

(defsyntax **
  lbp 140
  rbp 139
  led parse-infix)

(defsyntax :=
  led parse-infix
  header define
  lbp 80
  rbp 80)


(defsyntax /
  led parse-infix
  lbp 120
  rbp 120)

(defsyntax >
  led parse-infix
  lbp 80
  rbp 80)

(defsyntax <
  led parse-infix
  lbp 80
  rbp 80)

(defsyntax >=
  led parse-infix
  lbp 80
  rbp 80)

(defsyntax <=
  led parse-infix
  lbp 80
  rbp 80)

(defsyntax not
  nud parse-prefix
  lbp 70
  rbp 70)

(defsyntax #.QUOTE
  nud parse-prefix
  header quote)

(defsyntax and
  led parse-nary
  lbp 65)

(defsyntax or
  led parse-nary
  lbp 60)

(define (open-paren-nud token stream)
  (cond ((eq? (token-peek stream) '#.CLOSE-PAREN)
	 (token-read stream)
	 nil)
	('else
	 (prsmatch '#.CLOSE-PAREN '#.COMMA stream))))

(define (open-paren-led token left stream)
  (cons (header left) (prsmatch '#.CLOSE-PAREN '#.COMMA stream)))


(define (if-nud token stream)
  (define pred (parse (rbp token) stream))
  (define then (if (eq? (token-peek stream) 'then)
		   (parse (rbp (token-read stream)) stream)
		 (error 'missing-then)))
  (if (eq? (token-peek stream) 'else)
      (list 'if
	    pred
	    then
	    (parse (rbp (token-read stream)) stream))
    (list 'if
	  pred
	  then)))
