;;; (www data mime-types) --- Maintain a hash table of MIME types

;; Copyright (C) 2009 Thien-Thi Nguyen
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

(define-module (www data mime-types)
  #:export (reset-mime-types!
            put-mime-types-from-file!
            put-mime-types!
            mime-types<-extension
            select-extensions)
  #:autoload (ice-9 rdelim) (read-line)
  #:autoload (srfi srfi-13) (string-tokenize)
  #:autoload (srfi srfi-14) (char-set char-set-complement))

(define MT #f)                          ; data

;;; Support

(define (proc-error proc)               ; TODO: Move into Guile.
  (lambda (key fmt . args)
    (scm-error key (procedure-name proc) fmt args #f)))

(define (hasher proc resolve)

  (define (verr kind x)
    ((proc-error proc) (symbol-append 'invalid- kind)
     "Invalid ~A: ~S" kind x))

  (define (validate-extension x)
    (or (and (symbol? x))
        (verr 'extension x)))

  (define (validate-mime-type x)
    (or (and (symbol? x)
             (let* ((s (symbol->string x))
                    (pos (string-index s #\/)))
               (and pos (not (string-index s #\/ (1+ pos))))))
        (verr 'mime-type x)))

  (define (resolve<-resolve resolve)
    (define (b-list-w/o-c c b)
      (delq! c (if (symbol? b)
                   (list b)
                   b)))
    (define (conflict a b c)
      ((proc-error proc) 'mime-type-conflict
       "Conflict for extension: ~A~%  old: ~A~%  new: ~A" a b c))
    (case resolve
      ((error)  conflict)
      ((prefix) (lambda (a b c) (cons c (b-list-w/o-c c b))))
      ((suffix) (lambda (a b c) (append! (b-list-w/o-c c b) (list c))))
      ((stomp)  (lambda (a b c) c))
      ((quail)  (lambda (a b c) b))
      (else (verr 'resolve resolve))))

  (set! resolve (resolve<-resolve resolve))
  (lambda (ext mime-type)
    (validate-extension ext)
    (cond (mime-type
           (if (pair? mime-type)
               (for-each validate-mime-type mime-type)
               (validate-mime-type mime-type))
           (hashq-set! MT ext
                       (cond ((hashq-ref MT ext)
                              => (lambda (prev)
                                   (if (equal? prev mime-type)
                                       prev
                                       (resolve ext prev mime-type))))
                             (else mime-type))))
          (else (hashq-remove! MT ext)))))

;;; Public interface

;; Clear all entries from the mime-types hash table,
;; and prepare it for @var{size} (approximately) entries.
;; This procedure must be called before any others in this module.
;;
(define (reset-mime-types! size)
  (set! MT (make-hash-table size)))

;; Open @var{filename} and parse its contents as ``mime-types'' format.
;; This line-oriented file format is briefly described as follows:
;;
;; @itemize
;; @item Blank lines and lines beginning with @samp{#} are ignored.
;; @item Lines of the format @var{mime-type} (only one symbol) are ignored.
;; @item Otherwise, the line is expected to be in the format
;; @code{@var{mime-type} @var{extension} @var{extension}@dots{}},
;; that is, at least one @var{extension} must be present.  Each
;; @var{extension} results in an entry in the hash table.
;; @end itemize
;;
;; Put those those entries that specify an extension into the hash
;; table, validating both extension and mime-type first.
;; @var{resolve} specifies how to resolve extension conflicts.
;;
(define (put-mime-types-from-file! resolve filename)
  (let ((hash!! (hasher put-mime-types-from-file! resolve))
        (cs (char-set-complement (char-set #\space #\ht)))
        (p (open-input-file filename)))
    (let loop ((line (read-line p)))
      (or (eof-object? line)
          (begin
            (cond ((string-null? line))
                  ((char=? #\# (string-ref line 0)))
                  (else
                   (let* ((ls (delete "" (string-tokenize line cs)))
                          (mime-type (string->symbol (car ls))))
                     (for-each (lambda (ext)
                                 (hash!! ext mime-type))
                               (map string->symbol (cdr ls))))))
            (loop (read-line p)))))
    (close-port p)))

;; Put @var{extension1}/@var{mime-type1}@dots{} into the hash table,
;; validating both extension and mime-type first.
;; @var{resolve} specifies how to resolve extension conflicts.
;;
;; If an extension is given but there is no mime-type
;; (i.e., the list has an odd length), throw an error
;; with key @code{missing-mime-type}.
;;
;;-sig: (resolve [extension1 mime-type1 ...])
;;
(define (put-mime-types! resolve . rest)
  (let ((hash!! (hasher put-mime-types! resolve)))
    (let loop ((ls rest))
      (or (null? ls)
          (let ((ext (car ls)))
            (set! ls (cdr ls))
            (and (null? ls)
                 ((proc-error put-mime-types!) 'missing-mime-type
                  "Missing mime-type for extension: ~A" ext))
            (hash!! ext (car ls))
            (loop (cdr ls)))))))

;; Return the mime-type(s) associated with @var{ext} (a symbol or
;; string), or @code{#f} if none are found.  Note that generally the
;; value may be a single mime-type or a list of them.
;;
(define (mime-types<-extension ext)
  (let ((v (hashq-ref MT (if (string? ext)
                             (string->symbol ext)
                             ext))))
    (if (pair? v)
        ;; We are fascist implementors w/o a sense of humor.
        ;; (Apparently.)
        (list-copy v)
        v)))

;; Return a list of extensions in the hash table that match the
;; @var{sel} criteria (a symbol).  If @var{sel} is @code{#t}, return all
;; the extensions; if @code{single}, only those who have a single
;; mime-type associated; if @code{multiple}, only those who have more
;; than one mime-type associated.
;;
(define (select-extensions sel)
  (set! sel (case sel
              ((#t) (lambda (v) #t))
              ((single) symbol?)
              ((multiple) pair?)))
  (let* ((box (list #f))
         (tp box))
    (hash-for-each (lambda (k v)
                     (and (sel v)
                          (begin
                            (set-cdr! tp (list k))
                            (set! tp (cdr tp)))))
                   MT)
    (cdr box)))

;;; Load-time actions

(reset-mime-types! 3)
(apply put-mime-types! '(stomp
                         text text/plain
                         html text/html))

;;; (www data mime-types) ends here
