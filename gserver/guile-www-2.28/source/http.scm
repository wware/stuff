;;; (www http) --- HTTP client library

;; Copyright (C) 2008, 2009 Thien-Thi Nguyen
;; Copyright (C) 1997, 2001, 2002, 2003, 2004, 2005,
;;   2006, 2007 Free Software Foundation, Inc.
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

;; The (www http) module is fully documented in the guile-www.info file.

;;; Code:

(define-module (www http)
  #:export (http:message-version
            http:message-status-code
            http:message-status-text
            http:message-status-ok?
            http:status-ok?
            http:message-body
            http:message-headers
            http:message-header
            http:head
            http:get
            http:post-form
            http:connect
            http:open
            http:request)
  #:use-module ((srfi srfi-13) #:select ((substring/shared . subs)))
  #:use-module (www url)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 rw))


;;; Compatibility

(or (defined? 'read-line)
    (use-modules (ice-9 rdelim)))

(define (fs s . args)
  (apply simple-format #f s args))


;;; Variables that affect HTTP usage.

(define http:version "HTTP/1.0")

;; An HTTP message is represented by a vector:
;;      #(VERSION STATUS-CODE STATUS-TEXT HEADERS BODY)
;;
;; Each of VERSION, STATUS-CODE, STATUS-TEXT are strings.  HEADERS
;; is an alist of headers and their contents.  BODY is a single string.

(define (http:make-message version statcode stattext headers body)
  (vector version statcode stattext headers body))

;;;; HTTP status predicates.

;; Return the HTTP version in use in HTTP message @var{msg}.
(define (http:message-version msg)     (vector-ref msg 0))
;; Return the status code returned in HTTP message @var{msg}.
(define (http:message-status-code msg) (vector-ref msg 1))
;; Return the text of the status line from HTTP message @var{msg}.
(define (http:message-status-text msg) (vector-ref msg 2))
;; Return @code{#t} iff status code of @var{msg}
;; indicates a successful request.
(define (http:message-status-ok? msg)
  (http:status-ok? (http:message-status-code msg)))

;; Return @code{#t} iff @var{status} (a string) begins with "2".
(define (http:status-ok? status)
  (char=? #\2 (string-ref status 0)))

;; Return the body of the HTTP message @var{msg}.
(define (http:message-body msg) (vector-ref msg 4))

;; HTTP response headers functions
;;
;; An HTTP message header is represented here by a pair.  The CAR is a
;; symbol representing the header name, and the CDR is a string
;; containing the header text.  E.g.:
;;
;;      '((date . "Thu, 29 May 1997 23:48:27 GMT")
;;        (server . "NCSA/1.5.1")
;;        (last-modified . "Tue, 06 May 1997 18:32:03 GMT")
;;        (content-type . "text/html")
;;        (content-length . "8097"))
;;
;; Note: these symbols are all lowercase, although the original headers
;; were mixed-case.  Clients using this library should keep this in
;; mind, since Guile symbols are case-sensitive.
;;
;; FIXME: should headers with known semantics be parsed automatically?
;;   I.e. should the Content-Length header automatically get string->number?
;;   Should Date and Last-Modified headers be run through strptime?
;;   It is advantageous to keep headers in a uniform format, but it may
;;   be convenient to parse headers that have unambiguous meanings.

;; Return a list of the headers from HTTP message @var{msg}.
;;
(define (http:message-headers msg) (vector-ref msg 3))

;; Return the header field named @var{header} from HTTP message @var{msg},
;; or @code{#f} if no such header is present in the message.
;;
(define (http:message-header header msg)
  (http:fetch-header header (http:message-headers msg)))

(define (http:fetch-header header header-alist)
  (assq-ref header-alist header))

(define header-regex (make-regexp ": *"))

(define (http:header-parse hd)
  (let ((match (regexp-exec header-regex hd)))
    (cons (string->symbol
           (apply string
                  (map char-downcase
                       (string->list (match:prefix match)))))
          (match:suffix match))))

(define (parse-status-line statline)
  ;; Handle:     VERSION CODE
  ;; as well as: VERSION CODE TEXT
  ;; For the former, use the null string for TEXT.
  (let* ((first (string-index statline #\space))
         (second (string-index statline #\space (1+ first))))
    (list (subs statline 0 first)
          (subs statline (1+ first) (or second (string-length statline)))
          (if second
              (subs statline (1+ second))
              ""))))



;;; HTTP methods.

;; Common methods: HEAD, GET, POST etc.

;; Submit an http request using the @code{HEAD} method on the @var{url}.
;; The @code{Host} header is automatically included.
;;
(define (http:head url)
  (http:request "HEAD" url))

;; Submit an http request using the @code{GET} method on the @var{url}.
;; The @code{Host} header is automatically included.
;;
(define (http:get url)
  ;; FIXME: if http:open returns an old connection that has been
  ;; closed remotely, this will fail.
  (http:request "GET" url))

;; Submit an http request using the @code{POST} method on the @var{url}.
;; @var{extra-headers} is a list of extra headers, each a string of form
;; "@var{name}: @var{value} @dots{}".
;;
;; The "Content-Type" and "Host" headers are sent automatically and do
;; not need to be specified.  @var{fields} is a list of elements of the
;; form @code{(@var{fkey} . @var{fvalue})}, where @var{fkey} is a symbol
;; and @var{fvalue} is normally a string.
;;
;; @var{fvalue} can also be a list of file-upload specifications, each
;; of which has the form @code{(@var{source} @var{name} @var{mime-type}
;; @var{transfer-encoding})}.  @var{source} can be a string or a thunk
;; that returns a string.
;;
;; The rest of the elements are strings or symbols: @var{name} is the
;; filename (only the non-directory part is used); @var{mime-type} is a
;; type/subtype pair such as "image/jpeg", or @code{#f} to mean
;; "text/plain".  @var{transfer-encoding} is one of the tokens specified
;; by RFC 1521, or @code{#f} to mean "binary".  File-upload spec
;; elements with invalid types result in a "bad upload spec" error prior
;; to the http request.
;;
;; Note that @var{source} is used directly without further processing;
;; it is the caller's responsibility to ensure that the MIME type and
;; transfer encoding specified describe @var{source} accurately.
;;
(define (http:post-form url extra-headers fields)

  (define (source: spec)        (list-ref spec 0))
  (define (name: spec)          (list-ref spec 1))
  (define (mime-type: spec) (or (list-ref spec 2) "text/plain"))
  (define (xfer-enc: spec)  (or (list-ref spec 3) "binary"))

  (define (validate-upload-spec spec)
    (define (string-or-symbol? obj) (or (string? obj) (symbol? obj)))
    (or (and (list? spec)
             (= 4 (length spec))
             (and=> (source: spec) (lambda (source)
                                     (or (and (procedure? source)
                                              (equal? '(0 0 #f) ; thunk
                                                      (procedure-property
                                                       source 'arity)))
                                         (string? source))))
             (and=> (name: spec) string-or-symbol?)
             (and=> (mime-type: spec) string-or-symbol?)
             (and=> (xfer-enc: spec) string-or-symbol?))
        (error "bad upload spec:" spec)))

  (define (c-type type . boundary)
    (fs "Content-Type: ~A~A"
        type
        (if (null? boundary)
            ""
            (fs "; boundary=~S" (car boundary)))))

  (define (c-disp disp name . f?)
    (fs "Content-Disposition: ~A; ~Aname=\"~A\""
        disp (if (null? f?) "" "file") name))

  (let ((simple '()) (uploads '())      ; partition fields
        (boundary "gUiLeWwWhTtPpOsTfOrM"))
    (for-each (lambda (field)
                (if (pair? (cdr field))
                    (begin
                      (for-each validate-upload-spec (cdr field))
                      (set! uploads (cons field uploads)))
                    (set! simple (cons field simple))))
              fields)
    ;; reorder
    (set! simple (reverse! simple))
    (set! uploads (reverse! uploads))
    ;; do it!
    (http:request
     "POST" url
     ;; headers
     (cons
      (if (null? uploads)
          (c-type "application/x-www-form-urlencoded")
          (c-type "multipart/form-data" boundary))
      extra-headers)
     ;; body
     (if (null? uploads)
         (or (and (null? simple) simple)
             (let* ((enc (lambda (extract pair)
                           (url:encode (extract pair) '())))
                    (one (lambda (fmt pair)
                           (fs fmt (enc car pair) (enc cdr pair)))))
               (list                    ; all on one line
                (apply string-append
                       (one "~A=~A" (car simple))
                       (map (lambda (field)
                              (one "&~A=~A" field))
                            (cdr simple))))))
         (let ((boundary-line (string-append "--" boundary)))
           (define (aam proc ls)
             ;; note: PROC must cons else ‘append!’ will corrupt LS
             (apply append! (map proc ls)))
           `(,@(aam (lambda (pair)
                      (list
                       boundary-line
                       (c-disp "form-data" (car pair))
                       ""
                       (cdr pair)))
                    simple)
             ,@(aam (lambda (name-spec)
                      (let* ((sub-b (string-append "SuB" boundary))
                             (sub-b-line (string-append "--" sub-b)))
                        `(,boundary-line
                          ,(c-disp "form-data" (car name-spec))
                          ,(c-type "multipart/mixed" sub-b)
                          ""
                          ,@`(,@(aam (lambda (spec)
                                       (list
                                        sub-b-line
                                        (c-disp "attachment"
                                                (basename (name: spec))
                                                #t)
                                        (c-type (mime-type: spec))
                                        (fs "Content-Transfer-Encoding: ~A"
                                            (xfer-enc: spec))
                                        ""
                                        (let ((s (source: spec)))
                                          (if (string? s) s (s)))))
                                     (cdr name-spec)))
                          ,(string-append sub-b-line "--"))))
                    uploads)
             ,(string-append boundary-line "--")))))))

;; Connection-oriented functions:

;; Return a TCP stream socket connected to the location specified by
;; protocol @var{proto}, @var{addrfam} and @var{address}.  @var{proto}
;; is @code{PF_INET} or @code{PF_UNIX}, and the other args take
;; corresponding forms:
;;
;; @table @code
;; @item PF_INET
;; @code{(AF_INET @var{ipaddr} @var{portno})}, where @var{ipaddr} is
;; an integer.  Use @code{(car (hostent:addr-list (gethost @var{host})))}
;; to compute the ipaddr of @var{host} (a string).
;;
;; @item PF_UNIX
;; @code{(AF_UNIX @var{filename})}, made, for example, by@*
;; @code{(list AF_UNIX "/tmp/foo-control")}.
;; @end table
;;
;; Note that @code{PF_foo} and @code{AF_foo} are names of variables
;; that have constant values, not symbols.
;;
(define (http:connect proto addrfam address . address-rest)
  (let ((sock (socket proto SOCK_STREAM 0)))
    (apply connect sock addrfam address address-rest)
    sock))

;; Return an HTTP connection (a socket) to @var{host} (a string) on TCP
;; port @var{port} (default 80 if unspecified).
;;
;;-sig: (host [port])
;;
(define (http:open host . args)
  (let ((ipaddr (car (hostent:addr-list (gethost host))))
        (port (cond ((null? args) 80)
                    ((not (car args)) 80)
                    (else (car args)))))
    (http:connect PF_INET AF_INET ipaddr port)))

(define form-hack-regex
  ;; Normally ‘http:request’ formats the lines it sends over the socket to
  ;; end with CRLF, and correspondingly adjusts Content-Length upward by 2
  ;; for each line.  For ‘application/x-www-form-urlencoded’ messages with
  ;; only one one line in the body, however, this is inappropriate; often
  ;; the cgi at the other end of the socket misinterprets the CRLF as part
  ;; of the last value.  We help those programs avoid confusion by not
  ;; sending the CRLF (and munging Content-Length appropriately) for this
  ;; special case.
  (make-regexp "content-type: *application/x-www-form-urlencoded"
               regexp/icase))

;; Submit an HTTP request using @var{method} and @var{url}, wait
;; for a response, and return the response as an HTTP message object.
;;
;; @var{method} is the name of some HTTP method, e.g. "GET" or "POST".
;; @var{url} is a url object returned by @code{url:parse}.  Optional
;; args @var{headers} and @var{body} are lists of strings that comprise
;; the lines of an HTTP message.  The strings should not end with
;; @samp{CR} or @samp{LF} or @samp{CRLF}; @code{http:request} handles
;; that.  Also, the Content-Length header and Host header are calculated
;; automatically and should not be supplied.  Here are two examples:
;;
;; @example
;; (http:request "get" parsed-url
;;   (list "User-Agent: Anonymous/0.1"
;;         "Content-Type: text/plain"))
;;
;; (http:request "post" parsed-url
;;   (list "User-Agent: Fred/0.1"
;;         "Content-Type: application/x-www-form-urlencoded")
;;   (list (string-append "search=Gosper"
;;                        "&case=no"
;;                        "&max_hits=50")))
;; @end example
;;
;; As a special case (demonstrated in the second example above), when
;; Content-Type is @code{application/x-www-form-urlencoded} and there is
;; only one line in the body, the final @samp{CRLF} is omitted and the
;; Content-Length is adjusted accordingly.
;;
;;-sig: (method url [headers [body]])
;;
(define (http:request method url . args)
  (let ((host     (url:host url))
        (tcp-port (or (url:port url) 80))
        (path     (fs "/~A" (or (url:path url) ""))))
    (let ((sock (http:open host tcp-port))
          (request (fs "~A ~A ~A" method path http:version))
          (headers (cons (fs "Host: ~A" (url:host url))
                         (if (pair? args) (car args) '())))
          (body    (if (and (pair? args) (pair? (cdr args)))
                       (cadr args)
                       '())))
      (let* ((form-hack? (let loop ((ls headers))
                           (cond ((null? ls) #f)
                                 ((regexp-exec form-hack-regex (car ls))
                                  (and (pair? body)
                                       (not (pair? (cdr body)))))
                                 (else (loop (cdr ls))))))
             (content-length
              (apply +
                     (if form-hack? -2 0)
                     (map (lambda (line)
                            (+ 2 (string-length line))) ; + 2 for CRLF
                          body)))
             (headers (if (positive? content-length)
                          (cons (fs "Content-Length: ~A" content-length)
                                headers)
                          headers)))

        (with-output-to-port sock
          (lambda ()
            (display-with-crlf request)
            (for-each display-with-crlf headers)
            (display "\r\n")
            (if form-hack?
                (display (car body))
                (for-each display-with-crlf body))))

        ;; parse and add status line
        ;; also cons up a list of response headers
        (let* ((response-status-line (sans-trailing-whitespace
                                      (read-line sock 'trim)))
               (response-headers
                (let make-header-list ((ln (sans-trailing-whitespace
                                            (read-line sock 'trim)))
                                       (hlist '()))
                  (if (string-null? ln)
                      hlist
                      (make-header-list (sans-trailing-whitespace
                                         (read-line sock 'trim))
                                        (cons (http:header-parse ln)
                                              hlist)))))
               (response-status-fields
                (parse-status-line response-status-line))
               (response-version (car response-status-fields))
               (response-code    (cadr response-status-fields))
               (response-text    (caddr response-status-fields)))

          ;; signal error if HTTP status is invalid
          ;; (or (http:status-ok? response-code)
          ;; (error 'http-status "HTTP server returned bad status"
          ;;        response-status-line))
          ;; Get message body: if Content-Length header was supplied, read
          ;; that many chars.  Otherwise, read until EOF

          (let ((content-length (http:fetch-header
                                 'content-length
                                 response-headers)))
            (let ((response-body
                   (if (and content-length
                            (not (string-ci=? method "HEAD")))
                       (read-n-chars (string->number content-length) sock)
                       (with-output-to-string
                         (lambda ()
                           (while (not (eof-object? (peek-char sock)))
                             (display (read-char sock))))))))

              ;; FIXME: what about keepalives?
              (close-port sock)

              (http:make-message response-version
                                 response-code
                                 response-text
                                 response-headers
                                 response-body))))))))



;;; System interface cruft & string funcs

(define (read-n-chars num . port-arg)
  (let ((p (if (null? port-arg)
               (current-input-port)
               (car port-arg)))
        (s (make-string num)))
    (let loop ((start 0))
      (or (= start num)
          (let ((try (read-string!/partial s p start)))
            (and (number? try)
                 (loop (+ start try))))))
    s))

(define (display-with-crlf line)
  (display line)
  (display "\r\n"))

;; (sans-trailing-whitespace STR)
;;      These are defined in module (ice-9 string-fun), so this code
;;      will probably be discarded when the module system and boot-9
;;      settle down.

(define (sans-trailing-whitespace s)
  (let ((st 0)
        (end (string-length s)))
    (while (and (< 0 end)
                (char-whitespace? (string-ref s (1- end))))
      (set! end (1- end)))
    (if (< end st)
        ""
        (subs s st end))))

;;; (www http) ends here
