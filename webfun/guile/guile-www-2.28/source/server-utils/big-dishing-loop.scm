;;; (www server-utils big-dishing-loop) --- Customizable listener and dispatch

;; Copyright (C) 2008, 2009, 2010 Thien-Thi Nguyen
;; Copyright (C) 2004, 2005, 2006, 2007 Free Software Foundation, Inc.
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

;; The (www server-utils big-dishing-loop) module
;; is fully documented in the guile-www.info file.

;;; Code:

(define-module (www server-utils big-dishing-loop)
  #:export (named-socket
            echo-upath
            make-big-dishing-loop)
  #:use-module (ice-9 optargs)
  #:use-module (www server-utils parse-request)
  #:use-module (www server-utils answer))

;; Return a new socket in protocol @var{family} with address @var{name}.
;; Keywords are: @code{#:socket-setup}.
;;
;; First, evaluate @code{(socket @var{family} SOCK_STREAM 0)} to create
;; a new socket @var{sock}.  Next, handle @code{#:socket-setup}, with
;; value @var{setup}, like so:
;;
;; @table @asis
;; @item @code{#f}
;; Do nothing.  This is the default.
;;
;; @item @var{procedure}
;; Call @var{procedure} on @var{sock}.
;;
;; @item @code{((@var{opt} . @var{val}) @dots{})}
;; For each pair in this alist, call @code{setsockopt}
;; on @var{sock} with the pair's @var{opt} and @var{val}.
;; @end table
;;
;; Lastly, @code{bind} @var{sock} to @var{name}, which should be in a
;; form that is appopriate for @var{family}.  Two common cases are:
;;
;; @table @code
;; @item PF_INET
;; @code{(AF_INET @var{ipaddr} @var{portno})}, made, for example, by@*
;; @code{(list AF_INET INADDR_ANY 4242)}.
;;
;; @item PF_UNIX
;; @code{(AF_UNIX @var{filename})}, made, for example, by@*
;; @code{(list AF_UNIX "/tmp/foo-control")}.
;; @end table
;;
;; Note that @code{PF_foo}, @code{AF_foo}, and @code{INADDR_foo} are
;; names of variables that have constant values, not symbols.
;;
;;-sig: (family name [[keyword value] ...])
;;
(define* (named-socket family name #:key
                       (socket-setup #f))
  (let ((new (socket family SOCK_STREAM 0)))
    ((cond ((not socket-setup) identity)
           ((procedure? socket-setup) socket-setup)
           ((list? socket-setup)
            (lambda (sock)
              (for-each (lambda (pair)
                          (setsockopt sock SOL_SOCKET
                                      (car pair)
                                      (cdr pair)))
                        socket-setup)))
           (else
            (error "bad socket-setup:" socket-setup)))
     new)
    (apply bind new name)
    new))

;; Use mouthpiece @var{M} (@pxref{answer}) to compose and send a
;; "text/plain" response which has the given @var{upath} (a string)
;; and any @var{extra-args} as its content.  Shut down the socket
;; for both transmission and reception, then return @code{#t}.
;;
;; This proc can be used to
;; ensure basic network connectivity (i.e., aliveness testing).
;;
(define (echo-upath M upath . extra-args)
  (M #:set-reply-status:success)
  (M #:add-header #:Connection "close")
  (M #:add-header #:Content-Type "text/plain")
  (M #:add-content upath "\n")
  (for-each (lambda (arg)
              (M #:add-formatted "extra-arg: ~S\n" arg))
            extra-args)
  (M #:send-reply 2)
  #t)

(define http-hgrok (vector read-first-line
                           read-headers
                           skip-headers))

;; Return a proc @var{dish} that loops serving http requests from a socket.
;; @var{dish} takes one arg @var{ear}, which may be a pre-configured socket,
;; a TCP port number, or a list of the form:
;; @code{(@var{family} @var{address} @dots{})}.
;; When @var{ear} is a TCP port number, it is taken
;; to be the list @code{(PF_INET AF_INET INADDR_ANY @var{ear})}.
;;
;; In the latter two cases, the socket is realized by calling
;; @code{named-socket} with parameters @var{family} and @var{name} taken
;; from the @sc{car} and @sc{cdr}, respectively, of the list, with the
;; @code{#:socket-setup} paramater (see below) passed along unchanged.
;;
;; @var{dish} behavior is controlled by the keyword arguments given
;; to @code{make-big-dishing-loop}.  The following table is presented roughly
;; in order of the steps involved in processing a request, with default values
;; shown next to the keyword.
;;
;; @table @code
;; @findex socket-setup
;; @item #:socket-setup #f
;; This may be a proc that takes a socket, or a list of opt/val pairs which
;; are passed to @code{setsockopt}.  Socket setup is done for newly created
;; sockets (when @var{dish} is passed a TCP port number), prior to the
;; @code{bind} call.
;;
;; @findex queue-length
;; @item #:queue-length 0
;; The number of clients to queue, as set by the @code{listen} system call.
;; Setting the queue length is done for both new and pre-configured sockets.
;;
;; @findex concurrency
;; @item #:concurrency #:new-process
;; The type of concurrency (or none if the value is not recognized).
;; Here are the recognized values:
;;
;; @table @code
;; @item #:new-process
;; @itemx #:new-process/nowait
;; Fork a new process for each request.  The latter does not wait for the
;; child process to terminate before continuing the listen loop.
;;
;; @item #f
;; Handle everything in the current in process (no concurrency).
;; Unrecognized values are treated the same as @code{#f}.
;; @end table
;;
;; @findex bad-request-handler
;; @item #:bad-request-handler #f
;; If the first line of an HTTP message is not in the proper form, this
;; specifies a proc that takes a mouthpiece @var{m}.  Its return value should
;; be the opposite boston value of the @code{#:loop-break-bool} value, below.
;; @xref{answer}.
;;
;; @findex method-handlers
;; @item #:method-handlers ()
;; This alist describes how to handle the (valid) HTTP methods.
;; Each element has the form @code{(@var{method} . @var{handler})}.
;; @var{method} is a symbol, such as @code{GET}; and @var{handler} is
;; a procedure that handles the request for @var{method}.
;;
;; @var{handler} normally takes two arguments, the mouthpiece @var{m}
;; and the @var{upath} (string), composes and sends a response, and
;; returns non-@code{#f} to indicate that the big dishing loop should
;; continue.
;;
;; The proc's argument list is configured by @code{#:need-headers},
;; @code{#:need-input-port} and @code{#:explicit-return}.
;; Interpretation of the proc's return value is configured by
;; @code{#:explicit-return} and @code{#:loop-break-bool}.  See below.
;;
;; @findex need-headers
;; @item #:need-headers #f
;; @itemx #:need-input-port #f
;; If non-@code{#f}, these cause additional arguments to be supplied to the
;; handler proc.  If present, the headers arg precedes the input
;; port arg.  @xref{parse-request}.  The input port is always positioned at
;; the beginning of the HTTP message body.
;;
;; If @code{#:need-input-port} is @code{#f}, after the handler
;; proc returns, the port is @code{shutdown} in both (r/w) directions.  When
;; operating concurrently, this is done on the child side of the split.
;; @xref{Network Sockets and Communication,,,
;; guile, The Guile Reference Manual}.
;;
;; @findex explicit-return
;; @item #:explicit-return #f
;; If non-@code{#f}, this arranges for a continuation to be passed (as
;; the last argument) to the handler proc, and ignores that
;; proc's normal return value in favor of one explicitly passed through
;; the continuation.  If the continuation is not used, the
;; @dfn{effective return value} is computed as @code{(not
;; #:loop-break-bool)}.
;;
;; @findex loop-break-bool
;; @item #:loop-break-bool #f
;; Looping stops if the effective return value of the handler is
;; @code{eq?} to this value.
;;
;; @findex unknown-http-method-handler
;; @item #:unknown-http-method-handler #f
;; If @code{#f}, silently ignore unknown HTTP methods, i.e., those not
;; specified in @code{#:method-handlers}.
;; The value may also be a procedure that takes three
;; arguments: a mouthpiece @var{m}, the @var{method} (symbol) and the
;; @var{upath} (string).  Its return value should be the opposite boolean
;; value of the @code{#:loop-break-bool} value, below.
;; @xref{answer}.
;;
;; @findex parent-finish
;; @item #:parent-finish close-port
;; When operating concurrently (@code{#:concurrency} non-@code{#f}), the
;; ``parent'' applies this proc to the port after the split.
;;
;; @findex log
;; @item #:log #f
;; This proc is called after the handler proc returns.
;; Note that if @var{ear} is a unix-domain socket, the @var{client}
;; parameter will be simply "localhost".
;; @xref{log}.
;;
;; @findex status-box-size
;; @item #:status-box-size #f
;; This may be a non-negative integer, typically 0, 1 or 2.  It is used
;; by @code{#:log} (has no meaning if @code{#:log} is @code{#f}).
;; @xref{log}.
;;
;; @findex style
;; @item #:style #f
;; An object specifying the syntax of the first-line and headers.
;; The default specifies a normal HTTP message (@pxref{http}).
;; @end table
;;
;;-sig: ([keyword value ...])
;;
(define* (make-big-dishing-loop
          #:key
          (socket-setup #f)
          (style #f)
          (need-headers #f)
          (need-input-port #f)
          (explicit-return #f)
          (method-handlers '())
          (unknown-http-method-handler #f)
          (status-box-size #f)
          (loop-break-bool #f)
          (queue-length 0)
          (bad-request-handler #f)
          (concurrency #:new-process)
          (parent-finish close-port)
          (log #f))

  (or style (set! style http-hgrok))
  (let* ((hgrok-first-line (vector-ref style 0))
         (hgrok (cond ((and (not need-headers) (not need-input-port))
                       (lambda (port) #t))
                      (need-input-port (vector-ref style 1))
                      (else            (vector-ref style 2)))))

    (define (bdlcore queue-length sock handle-request)
      (listen sock queue-length)
      (let loop ((conn (accept sock)))
        (and (handle-request conn (hgrok-first-line (car conn)))
             (loop (accept sock)))))

    (define (handle-request conn upath method)
      (let* ((p (car conn))
             ;; headers
             (h (hgrok p))
             ;; status box
             (b (and (number? status-box-size)
                     (make-list status-box-size #f)))
             (M (mouthpiece p b))
             (res (cond ((assq-ref method-handlers method)
                         => (lambda (mh)
                              (call-with-current-continuation
                               (lambda (k)
                                 (apply mh M upath
                                        (append
                                         (if need-headers    (list h) '())
                                         (if need-input-port (list p) '())
                                         (if explicit-return (list k) '())))
                                 (not loop-break-bool)))))
                        (unknown-http-method-handler
                         => (lambda (umh)
                              (umh M method upath)))
                        (else
                         (not loop-break-bool)))))
        (and log (log (let* ((sock (cdr conn))
                             (fam (sockaddr:fam sock)))
                        (cond ((= PF_INET fam)
                               (let ((addr (sockaddr:addr sock))
                                     (port (sockaddr:port sock)))
                                 (simple-format #f "~A:~A"
                                                (inet-ntoa addr)
                                                port)))
                              ((= PF_UNIX fam)
                               (let ((fn (sockaddr:path sock)))
                                 (if (string-null? fn)
                                     "localhost"
                                     fn)))
                              (else
                               (object->string sock))))
                      method upath b))
        ;; return #t => keep going
        (not (eq? loop-break-bool res))))

    ;; rv
    (lambda (ear)
      (bdlcore
       queue-length

       (if (port? ear)
           ear
           (let ((int? (integer? ear)))
             (or int? (pair? ear)
                 (error "bad ear:" ear))
             (named-socket (if int?
                               PF_INET
                               (car ear))
                           (if int?
                               (list AF_INET INADDR_ANY ear)
                               (cdr ear))
                           #:socket-setup socket-setup)))

       (lambda (conn req)
         (let ((p (car conn)))

           (define (child)
             (let ((rv (cond (req
                              (apply handle-request conn (cdr (reverse! req))))
                             (bad-request-handler
                              (bad-request-handler (mouthpiece p)))
                             (else
                              (not loop-break-bool)))))
               (or need-input-port (shutdown p 2))
               rv))

           (case concurrency
             ((#:new-process #:new-process/nowait)
              (let ((pid (primitive-fork)))
                (cond ((zero? pid)
                       (exit (child)))
                      (else
                       (parent-finish p)
                       (set! p #f)
                       (or (eq? #:new-process/nowait concurrency)
                           (zero? (status:exit-val (cdr (waitpid pid)))))))))
             (else
              (child)))))))))

;;; (www server-utils big-dishing-loop) ends here
