;; Guile Scheme has POSIX bindings including socket programming, so let's try
;; to write a little HTTP server.

;; What's needed next is a series of small example websites. These should be
;; little Scheme files which, when you execute them, run a server process and
;; illustrate various things you can do. When executed, they should print to
;; standard output some instructions about how to visit them from a browser or
;; where to find client code that uses them.

;; There is a serious lack of readable documentation about guile-www on the
;; web. I'm particularly perplexed by the server loop, so let's write one
;; here, and hope it sheds light on the guile-www version.

(define portnum 8800)

(display "To test this, type:") (newline)
(display "  telnet 127.0.0.1 ") (display portnum) (newline)
(display "Then type a short phrase and hit Enter") (newline)

(let ((sock (socket AF_INET SOCK_STREAM 6))
      (addr (+ (* 127 256 256 256) 1))
      (portnum 8800))
  (bind sock AF_INET addr portnum)
  (listen sock 1)
  (do ()
      (#f '())
    (let* ((req (accept sock))
	   (conn (car req))
	   (info (cdr req)))
      ;; (display info)
      (newline)
      (let* ((str "01234567890123456789012345678901234567890123456789")
	     (n (recv! conn str))
	     (shorter (substring str 0 n)))
	(display shorter)
	(send conn "Thanks!\r\n")
	(close-port conn)))))
