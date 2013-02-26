;;; (www server-utils parse-request) --- Read HTTP first line and headers

;; Copyright (C) 2009 Thien-Thi Nguyen
;; Copyright (C) 2004 Free Software Foundation, Inc.
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

(define-module (www server-utils parse-request)
  #:export (read-first-line
            hqf<-upath alist<-query
            read-headers skip-headers read-body)
  #:autoload (www url-coding) (url-coding:decode)
  #:use-module (srfi srfi-13)
  #:use-module (srfi srfi-14)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 rw)
  #:use-module (ice-9 and-let-star))

(define (read-line/CRLF port)
  (and-let* ((pair (read-line port 'split))
             ((eq? #\newline (cdr pair)))
             (raw (car pair))
             (lc-pos (1- (string-length raw)))
             ((not (negative? lc-pos)))
             ((eq? #\cr (string-ref raw lc-pos))))
    (substring/shared raw 0 lc-pos)))

;; Parse the first line of the HTTP message from input @var{port} and
;; return a list of the method, URL path and HTTP version indicator, or
;; @code{#f} if the line ends prematurely or is otherwise malformed.  A
;; successful parse consumes the trailing @samp{CRLF} of the line as
;; well.  The method is a symbol with its constituent characters
;; upcased, such as @code{GET}; the other elements are strings.  If the
;; first line is missing the HTTP version, @code{parse-first-line}
;; returns the default "HTTP/1.0".
;;
(define (read-first-line port)
  (and-let* ((line (read-line/CRLF port))
             (len (string-length line))
             (eom (string-index line char-set:whitespace))
             (bou (string-skip  line char-set:whitespace eom))
             (eou (or (string-index line char-set:whitespace bou) len))
             (sub (lambda x (apply substring/shared line x))))
    (list (string->symbol (sub 0 eom))
          (sub bou eou)
          (if (= len eou)
              "HTTP/1.0"
              (sub (string-skip line char-set:whitespace eou))))))

;; Parse @var{upath} and return three values representing
;; its hierarchy, query and fragment components.
;; If a component is missing, its value is @code{#f}.
;;
;; @example
;; (hqf<-upath "/aa/bb/cc?def=xyz&hmm#frag")
;; @result{} #<values "/aa/bb/cc" "def=xyz&hmm" "frag">
;;
;; (hqf<-upath "/aa/bb/cc#fr?ag")
;; @result{} #<values "/aa/bb/cc" #f "fr?ag">
;; @end example
;;
(define (hqf<-upath upath)
  (define (bit . x)
    (apply substring/shared upath x))
  (or (and-let* ((one (string-index upath (char-set #\? #\#)))
                 (h (bit 0 one))
                 (more (1+ one)))
        (and (string-null? h)
             (set! h #f))
        (cond ((char=? #\# (string-ref upath one))
               (values h #f (bit more)))
              ((string-index upath #\# more)
               => (lambda (two)
                    (values h (bit more two) (bit (1+ two)))))
              (else
               (values h (bit more) #f))))
      (values upath #f #f)))

(define amp-split
  (let ((not-amp-cs (char-set-complement (char-set #\&))))
    (lambda (s)
      (string-tokenize s not-amp-cs))))

;; Parse urlencoded @var{query-string} and return an alist.
;; For each element @code{(@var{name} . @var{value})} of the alist,
;; @var{name} is a string and @var{value} is either @code{#f} or a string.
;;
(define (alist<-query query-string)
  (map (lambda (pair)
         (define (decode . args)
           (url-coding:decode (apply substring/shared pair args)))
         (let ((mid (string-index pair #\=)))
           (cons (if mid (decode 0 mid) (decode 0))
                 (and mid (decode (1+ mid))))))
       (amp-split query-string)))

;; Parse the headers of the HTTP message from input @var{port} and
;; return a list of key/value pairs, or @code{#f} if the message ends
;; prematurely or is otherwise malformed.  Both keys and values are
;; strings.  Values are trimmed of leading and trailing whitespace and
;; may be empty.  Values that span more than one line have their
;; "continuation whitespace" reduced to a single space.  A successful
;; parse consumes the trailing @samp{CRLF} of the header block as well.
;;
(define (read-headers port)
  (let loop ((acc '()))
    (and-let* ((line (read-line/CRLF port)))
      (cond ((string-null? line)
             (map (lambda (pair)
                    (let ((v (cdr pair)))
                      (if (string? v)
                          pair
                          (cons (car pair) (string-join (reverse! v) " ")))))
                  (reverse! acc)))
            ((char-set-contains? char-set:whitespace (string-ref line 0))
             (and-let* (((not (null? acc)))
                        (more (string-trim-both line)))
               (or (string-null? more)
                   (let* ((prev (car acc))
                          (pls (cdr prev)))
                     (set-cdr! prev (cons more (if (string? pls)
                                                   (list pls)
                                                   pls)))))
               (loop acc)))
            (else
             (and-let* ((colon (string-index line #\:)))
               (loop (acons
                      (string-trim-right line char-set:whitespace 0 colon)
                      (string-trim-both line char-set:whitespace (1+ colon))
                      acc))))))))

;; Scan without parsing the headers of the HTTP message from input
;; @var{port}, and return the empty list, or @code{#f} if the message
;; ends prematurely.  A successful scan consumes the trailing
;; @samp{CRLF} of the header block as well.
;;
(define (skip-headers port)
  (let loop ()
    (and-let* ((line (read-line/CRLF port)))
      (if (string-null? line)
          '()
          (loop)))))

;; Return a new string of @var{len} bytes with contents
;; read from input @var{port}.
;;
(define (read-body len port)
  (let ((s (make-string len)))
    (let loop ((start 0))
      (or (= start len)
          (let ((try (read-string!/partial s port start)))
            (and (number? try)
                 (loop (+ start try))))))
    s))

;;; (www server-utils parse-request) ends here
