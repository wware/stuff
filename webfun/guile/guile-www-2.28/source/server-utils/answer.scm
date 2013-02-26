;;; (www server-utils answer) --- HTTP connection handling and responses

;; Copyright (C) 2008, 2009, 2010 Thien-Thi Nguyen
;; Copyright (C) 2004, 2006, 2007 Free Software Foundation, Inc.
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

(define-module (www server-utils answer)
  #:export (CRLF flat-length fs walk-tree tree-flat-length! string<-tree
                 string<-headers
                 string<-header-components
                 mouthpiece)
  #:use-module (ice-9 optargs)
  #:use-module ((ice-9 rw) #:select (write-string/partial)))

(define-macro (+! v n)
  `(set! ,v (+ ,v ,n)))

(define CRLF "\r\n")

(define flat-length (make-object-property))

;; Return a new string made by using format string @var{s} on @var{args}.
;; As in @code{simple-format} (which this procedure uses), @code{~A} expands
;; as with @code{display}, while @code{~S} expands as with @code{write}.
;;
(define (fs s . args)
  (apply simple-format #f s args))

;; Call @var{proc} for each recursively-visited leaf in @var{tree}, excluding
;; empty lists.  It is an error for @var{tree} to contain improper lists.
;;
(define (walk-tree proc tree)
  (cond ((null? tree))
        ((pair? tree) (for-each (lambda (sub) (walk-tree proc sub)) tree))
        (else (proc tree))))

;; If @var{tree} is a string, return its @code{string-length}.
;; If @var{tree} already has a @code{flat-length}, return that.
;; Otherwise, recursively compute, set, and return the
;; @code{flat-length} of @var{tree}.
;;
(define (tree-flat-length! tree)
  (cond ((string? tree) (string-length tree))
        ((null? tree) 0)
        ((flat-length tree))
        (else (set! (flat-length tree)
                    (+ (tree-flat-length! (car tree))
                       (tree-flat-length! (cdr tree)))))))

;; Return a new string made from flattening @var{tree}.
;; Set the @code{flat-length} (using @code{tree-flat-length!})
;; of @var{tree} by side effect.
;;
(define (string<-tree tree)
  (let ((wp 0)
        (rv (make-string (tree-flat-length! tree))))
    (walk-tree (lambda (s)
                 (let ((len (string-length s)))
                   (substring-move! s 0 len rv wp)
                   (+! wp len)))
               tree)
    rv))

(define ((ish-ref idx) ish) (vector-ref ish idx))

(define ish-status   (ish-ref 0))
(define ish-h-k-end  (ish-ref 1))
(define ish-h-v-end  (ish-ref 2))
(define ish-neck     (ish-ref 3))

(define http-ish (vector "HTTP/1.0 ~A ~A\r\n"
                         ": " CRLF
                         CRLF))

(define (tree<-header-proc style)       ; => (lambda (key val) ...)
  (define (k x)
    (if (string? x)
        x
        (fs "~A" (if (keyword? x)
                     (keyword->symbol x)
                     x))))
  (define (v x)
    (if (or (string? x) (pair? x) (null? x))
        x
        (fs "~A" x)))
  (let* ((one (ish-h-k-end style))
         (two (ish-h-v-end style))
         (k-len (+ (string-length one)
                   (string-length two))))
    ;; rv
    (lambda (key val)
      (set! key (k key))
      (set! val (v val))
      (let ((tree (list key one
                        val two)))
        (set! (flat-length tree) (+ (string-length key)
                                    (tree-flat-length! val)
                                    k-len))
        tree))))

;; Return a string made from formatting name/value pairs in @var{alist},
;; according to the optional @code{style} argument.  If unspecified or
;; specified as @code{#f}, the default is to format headers like so:
;;
;; @example
;; NAME #\: #\space VALUE #\cr #\lf
;; @end example
;;
;; Each name may be a string, symbol or keyword.  Each value may be a
;; string, number, symbol, or a tree.
;;
;;-sig: (alist [style])
;;
(define* (string<-headers alist #:optional (style #f))
  (string<-tree (map (tree<-header-proc (or style http-ish))
                     (map car alist)
                     (map cdr alist))))

;; Return a string made from formatting header name @var{n} and value
;; @var{v}.  Additional headers can be specified as alternating name and
;; value args.  Each header is formatted like so: ``@var{name}:
;; @var{value}\r\n''.
;;
;; Each @var{n} may be a string, symbol or keyword.  Each @var{v} may be a
;; string, number, symbol, or a tree.
;;
;; @strong{NOTE}: This proc @strong{will be removed} after 2011-12-31.
;; Use @code{string<-headers} instead.
;;
;;-sig: (n v [n1 v1...])
;;
(define (string<-header-components . ls)
  (string<-headers
   (let loop ((acc '()) (ls ls))
     (if (null? ls)
         (reverse! acc)
         (loop (acons (car ls) (cadr ls) acc)
               (cddr ls))))))

;; Return a command-delegating closure capable of writing a properly formatted
;; HTTP 1.0 response to @var{out-port}.  Optional arg @var{status-box} is a
;; list whose @sc{car} is set to the numeric status code given to a
;; @code{#:set-reply-status} command.  If @var{status-box} has length of two
;; or more, its @sc{cadr} is set to the content-length on @code{#:send-reply}.
;; A content-length value of @code{#f} means there have been no calls to
;; @code{#:add-content}.  The commands and their args are:
;;
;; @table @code
;; @findex reset-protocol!
;; @item #:reset-protocol!
;; Reset internal state, including reply status, headers and content.
;; This is called automatically by @code{#:send-reply}.
;;
;; @findex set-reply-status
;; @item #:set-reply-status @var{number} @var{message}
;; Set the reply status.  @var{message} is a short string.
;;
;; @findex set-reply-status:success
;; @item #:set-reply-status:success
;; This is equivalent to @code{#:set-reply-status 200 "OK"}.
;;
;; @findex add-header
;; @item #:add-header @var{name} @var{value}
;; @var{name} may be @code{#f}, @code{#t}, a string, symbol or keyword.
;; @var{value} is a string.  If @var{name} is @code{#f} or @code{#t},
;; @var{value} is taken to be a pre-formatted string, "A: B" or "A:
;; B\r\n", respectively.  If @var{name} is not a boolean, @var{value}
;; may also be a tree of strings or a number.
;;
;; @findex add-content
;; @item #:add-content [@var{tree} @dots{}]
;; @var{tree} may be a string, a nested list of strings, or a series of such.
;; Subsequent calls to @code{#:add-content} append their trees to the
;; collected content tree thus far.
;;
;; @findex add-formatted
;; @item #:add-formatted @var{format-string} [@var{args} @dots{}]
;; @var{format-string} may be @code{#f} to mean @code{~S}, @code{#t} to
;; mean @code{~A}, or a normal format string.  It is used to format
;; @var{args}, and the result passed to @code{#:add-content}.
;;
;; @findex add-direct-writer
;; @item #:add-direct-writer @var{len} @var{write}
;; @var{len} is the number of bytes that procedure @var{write} will
;; output to its arg, @var{out-port} (passed back), when called during
;; @code{#:send-reply}.  This is to allow sendfile(2) and related
;; hackery.
;;
;; @findex content-length
;; @item #:content-length
;; Return the total number of bytes in the content added thus far.
;;
;; @findex rechunk-content
;; @item #:rechunk-content @var{chunk}
;; @var{chunk} may be @code{#f}, in which case a list of the string
;; lengths collected thus far is returned; @code{#t} which means to use
;; the content length as the chunk size (effectively producing one
;; chunk); or a number specifying the maximum size of a chunk.  The
;; return value is a list of the chunk sizes.
;;
;; It is an error to use @code{#:rechunk-content} with a non-@code{#f}
;; @var{chunk} in the presence of a previous @code{#:add-direct-writer}.
;;
;; @findex inhibit-content!
;; @item #:inhibit-content! @var{bool}
;; Non-@code{#f} @var{bool} arranges for @code{#:send-reply} (below) to
;; compute content length and add the appropriate header, as usual, but
;; no content is actually sent.  This is useful, e.g., when answering a
;; @code{HEAD} request.  If @var{bool} is @code{#f}, @code{#:send-reply}
;; acts normally (i.e., sends both headers and content).
;;
;; @findex send-reply
;; @item #:send-reply [close]
;; Send the properly formatted response to @var{out-port}, and reset
;; all internal state (status reset, content discarded, etc).  It is
;; an error to invoke @code{#:send-reply} without having first set
;; the reply status.
;;
;; Optional arg @var{close} means do a @code{shutdown} on @var{out-port}
;; using @var{close} --- directly, if an integer, or called with no
;; arguments, if a thunk --- as the shutdown @code{how} argument.
;; (Note: If @var{out-port} is not a socket, this does nothing silently.)
;; @xref{Network Sockets and Communication,,,guile}.
;;
;; If @var{close} is specified, the closure forgets about @var{out-port}
;; internally; it is an error to call other mouthpiece commands,
;; subsequently.
;; @end table
;;
;;-sig: (out-port [status-box [style]])
;;
(define* (mouthpiece out-port #:optional (status-box '()) (style #f))

  (and (or (not status-box)
           (and (not (null? status-box))         ; normalize
                (not (list? (car status-box)))))
       (set! status-box '()))
  (or style (set! style http-ish))

  (let* ((pre-tree (list #f))
         (pre-tp pre-tree)
         (pre-len 0)
         (preamble (make-string (- 1024 16)))
         (tree<-header (tree<-header-proc style))
         (status-number! (if (null? status-box)
                             identity
                             (let ((place (car status-box)))
                               (lambda (number)
                                 (set-car! place number)))))
         (status-content-length! (if (or (null? status-box)
                                         (null? (cdar status-box)))
                                     identity
                                     (let ((place (cdar status-box)))
                                       (lambda (length)
                                         (set-car! place length)))))
         (inhibit-content? #f)
         (direct-writers '())
         (content '())
         (content-length #f))

    (define (reset-protocol!)
      (set! pre-tree (list #f))
      (set! pre-tp pre-tree)
      (set! pre-len 0)
      (set! inhibit-content? #f)
      (set! direct-writers '())
      (set! content '())
      (set! content-length #f))

    (define (set-reply-status number msg)
      (status-number! number)
      (let ((s (fs (ish-status style) number msg)))
        (+! pre-len (string-length s))
        (set-car! pre-tree s)))

    (define (set-reply-status:success)
      (set-reply-status 200 "OK"))

    (define (preamble-append! len new)
      (+! pre-len len)
      (set-cdr! pre-tp (list new)))

    (define (add-header name value)
      (define (up! new)
        (preamble-append! (tree-flat-length! new) new)
        (set! pre-tp (cdr pre-tp)))
      (cond ((eq? #f name)
             (up! (list value CRLF)))
            ((eq? #t name)
             (up! value))
            (else
             (up! (tree<-header name value)))))

    (define (add-content . tree)
      (or content-length (set! content-length 0))
      (+! content-length (tree-flat-length! tree))
      (set! content (append! content tree)))

    (define (add-formatted fstr . args)
      (add-content (list (apply fs
                                (cond ((eq? #f fstr) "~S")
                                      ((eq? #t fstr) "~A")
                                      (else fstr))
                                args))))

    (define (add-direct-writer len write)
      (or content-length (set! content-length 0))
      (+! content-length len)
      (set! direct-writers (acons write len direct-writers))
      (set! content (append! content (list write))))

    (define (rechunk-content chunk)
      (define (upd! proc get-new-content)
        (walk-tree proc content)
        (set! content (get-new-content)))
      (cond ((eq? #f chunk)
             (let ((ls '()))
               (walk-tree (lambda (x)
                            (set! ls (cons (if (procedure? x)
                                               (assq-ref direct-writers x)
                                               (string-length x))
                                           ls)))
                          content)
               (reverse ls)))
            ((not (null? direct-writers))
             (error "cannot rechunk in the presence of direct-writers"))
            ((eq? #t chunk)
             (rechunk-content content-length))
            ((and (number? chunk) (not (< 0 chunk)))    ;;; slack
             '())
            ((number? chunk)
             (let* ((extra (remainder content-length chunk))
                    (dreck (make-list (quotient content-length chunk) chunk))
                    (frizz (if (zero? extra)
                               dreck
                               (reverse (cons extra dreck))))
                    (noise (map (lambda (n) (make-string n)) frizz))
                    (nw noise)
                    (dest (car nw))
                    (dlen (string-length dest))
                    (wpos 0))
               (upd! (lambda (s)
                       (let* ((size (string-length s))
                              (dpos (remainder wpos chunk))
                              (left (- dlen dpos)))
                         (let loop ((start 0) (move (min size left)))
                           (substring-move! s start (+ start move)
                                            dest dpos)
                           (+! wpos move)
                           (let ((new-start (+ start move))
                                 (new-dpos (remainder wpos chunk)))
                             (cond ((zero? new-dpos)
                                    (set! nw (cdr nw))
                                    (cond ((not (null? nw))
                                           (set! dest (car nw))
                                           (set! dlen (string-length dest))))))
                             (or (= size new-start)
                                 (begin
                                   (set! dpos new-dpos)
                                   (set! left (- dlen dpos))
                                   (loop new-start
                                         (min (- size new-start)
                                              left))))))))
                     (lambda () noise))
               frizz))
            (else
             (error "chunk must be #f, #t or a number:" chunk))))

    (define (inhibit-content! value)
      (set! inhibit-content? (->bool value)))

    (define (send-reply . close)
      (define (out! s stop)
        (let loop ((start 0))
          (set! start (+ start (write-string/partial s out-port start stop)))
          (or (= start stop)
              (loop start))))
      (or (car pre-tree) (error "reply status not set"))
      (and content-length (add-header #:Content-Length content-length))
      (let ((neck (ish-neck style)))
        (preamble-append! (string-length neck) neck))
      (and (< (string-length preamble) pre-len)
           (set! preamble (make-string (+ pre-len 64))))
      (let ((wp 0))
        (walk-tree (lambda (s)
                     (let ((len (string-length s)))
                       (substring-move! s 0 len preamble wp)
                       (+! wp len)))
                   pre-tree))
      (out! preamble pre-len)
      (or inhibit-content?
          (walk-tree (lambda (x)
                       (if (procedure? x)
                           (x out-port)
                           (out! x (string-length x))))
                     content))
      (force-output out-port)
      (status-content-length! (if inhibit-content? 0 content-length))
      (reset-protocol!)
      (or (null? close)
          (let ((close (car close)))
            (and (eq? 'socket (port-filename out-port))
                 (shutdown out-port (if (thunk? close)
                                        (close)
                                        close)))
            (set! out-port #f))))

    ;; rv
    (lambda (command . args)
      (or (keyword? command) (error "command not a keyword:" command))
      (apply
       (case command
         ((#:reset-protocol!) reset-protocol!)
         ((#:set-reply-status) set-reply-status)
         ((#:set-reply-status:success) set-reply-status:success)
         ((#:add-header) add-header)
         ((#:add-content) add-content)
         ((#:add-formatted) add-formatted)
         ((#:add-direct-writer) add-direct-writer)
         ((#:content-length) (lambda () content-length))
         ((#:rechunk-content) rechunk-content)
         ((#:inhibit-content!) inhibit-content!)
         ((#:send-reply) send-reply)
         (else (error "unrecognized command:" command)))
       args))))

;;; (www server-utils answer) ends here
