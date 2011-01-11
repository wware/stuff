;;; (www server-utils filesystem) --- Work with the local filesystem

;; Copyright (C) 2009, 2010 Thien-Thi Nguyen
;; Copyright (C) 2004, 2005 Free Software Foundation, Inc.
;; Copyright (C) 2000, 2001 Martin Grabmueller
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

;; The (www server-utils filesystem) module
;; is fully documented in the guile-www.info file.

;;; Code:

(define-module (www server-utils filesystem)
  #:export (access-forbidden?-proc
            cleanup-filename
            upath->filename-proc
            filename->content-type)
  #:use-module ((srfi srfi-13) #:select (string=
                                         (substring/shared . subs)
                                         string-prefix?
                                         string-suffix?
                                         string-tokenize
                                         string-join))
  #:use-module ((srfi srfi-14) #:select (char-set-complement
                                         char-set))
  #:use-module (ice-9 and-let-star)
  #:autoload (www data content-type) (*content-type-by-filename-extension*)
  #:autoload (www data mime-types) (mime-types<-extension))

;; Create and return a filesystem-access procedure based on
;; @var{docroot} and @var{forbid-rx}.  The returned procedure @var{p}
;; takes a @var{filename} and returns @code{#t} if access to that file
;; should be denied for any of the following reasons:
;;
;; @itemize
;; @item @var{filename} does not begin with @var{docroot}
;; @item @var{filename} matches regular expression @var{forbid-rx}
;; @end itemize
;;
;; If @var{forbid-rx} is @code{#f}, the regular expression check is
;; skipped.  @var{p} returns @code{#f} if access should be granted.
;;
(define (access-forbidden?-proc docroot forbid-rx)
  (let ((rx (and forbid-rx (make-regexp forbid-rx))))
    ;; rv
    (lambda (filename)
      (or (not (string-prefix? docroot filename))
          (and rx (regexp-exec rx filename))))))

(define +not-slash+ (char-set-complement (char-set #\/)))

;; Return a new filename made from cleaning up filename @var{name}.
;; Cleaning up is a transform that collapses each of these, in order:
;;
;; @itemize
;; @item @samp{//}
;; @item @samp{/./}
;; @item @samp{/@var{foo}/../}
;; @end itemize
;;
;; into a single slash (@samp{/}), everywhere in @var{name}, plus some
;; fixups.  The transform normally preserves the trailing slash (if any)
;; in @var{name}, and does not change any leading @samp{..} components
;; if @var{name} is relative, i.e., does not begin with slash.  Due to
;; proper @samp{/@var{foo}/../} cancellation for relative @var{name},
;; however, the result may be the empty string.  (Here, @dfn{proper}
;; means that @var{foo} is not @samp{..}, but a normal filename
;; component.)
;;
(define (cleanup-filename name)
  ;; TODO: Write test to guard against regression; reimplement.
  ;;       See (info "(guile-www) filesystem") for test cases.
  (define (not-dot-dot ls)
    (not (string= ".." (car ls))))
  (let* ((abs? (string-prefix? "/" name))
         (comps (string-tokenize name +not-slash+))
         (end-slash? (string-suffix? "/" name))
         (dir? (or end-slash? (and (not (null? comps))
                                   (member (car (last-pair comps))
                                           '("." "..")))))
         (head-dd (let loop ((n 0))
                    (cond ((or (null? comps) (not-dot-dot comps))
                           (set! comps (delete "." comps))
                           n)
                          (else
                           (set! comps (cdr comps))
                           (loop (1+ n)))))))
    (let loop ((rev (if abs? '() (make-list head-dd ".."))))
      (if (null? comps)
          (string-join (reverse! (if (and dir? (or end-slash?
                                                   (not (pair? rev))
                                                   (not-dot-dot rev)))
                                     (cons "" rev)
                                     rev))
                       "/"
                       (if abs? 'prefix 'infix))
          (let ((one (car comps)))
            (set! comps (cdr comps))
            (loop (cond ((not (string= ".." one))
                         (cons one rev))
                        ((pair? rev)
                         (cdr rev))
                        (abs?
                         rev)
                        (else
                         (cons one rev)))))))))

;; Create and return a url-path-to-filename mapping procedure based on
;; @var{docroot}.  The returned procedure @var{p} takes a (string)
;; @var{upath} and returns a valid local filename path for the requested
;; resource, or @code{#f} if that file cannot be found.  Optional arg
;; @var{dir-indexes} specifies an ordered list of filenames to try if
;; the resolved filename path turns out to be a directory.
;;
;; If no such files exist, return the directory name.  As a special
;; case, when @var{p} encounters a value of @code{#f} during iteration
;; over @var{dir-indexes}, it returns @code{#f} immediately.
;;
;; For example, presuming files @file{/a/b/c.txt} and
;; @file{/a/b/index.html} both exist and are readable:
;;
;; @example
;; (define resolve (upath->filename-proc
;;                    "/a/b/"
;;                    '("index.shtml" "index.html")))
;;
;; (resolve "/random") @result{} #f
;; (resolve "/c.txt") @result{} "/a/b/c.txt"
;; (resolve "/") @result{} "/a/b/index.html"
;; @end example
;;
;; Directory names are always returned with a trailing slash.
;;
;;-sig: (docroot [dir-indexes])
;;
(define (upath->filename-proc docroot . idx)
  (or (null? idx) (set! idx (car idx)))
  ;; rv
  (lambda (upath)
    (let ((filename (cleanup-filename (in-vicinity docroot upath))))
      (and (file-exists? filename)
           (case (stat:type (stat filename))
             ((regular) filename)
             ((directory)
              (set! filename (in-vicinity filename "")) ; ensure trailing /
              (let loop ((ls idx))
                (cond ((null? ls)
                       filename)
                      ((eq? #f (car ls))
                       #f)
                      (else
                       (let ((full (string-append filename (car ls))))
                         (or (and (file-exists? full)
                                  (eq? 'regular (stat:type (stat full)))
                                  full)
                             (loop (cdr ls))))))))
             (else #f))))))

;; Return a valid Content-Type string which matches @var{filename} best.
;; Matching is done by comparing the extension (part of @var{filename} after
;; the last "." if available) against a table.  If none match, return
;; "application/octet-stream".  Optional arg @var{default} specifies another
;; value to use instead of "application/octet-stream".
;;
;; If there are multiple MIME types associated with the extension,
;; return the first one.
;;
;; @xref{mime-types}, proc @code{put-mime-types!}, for more info.
;;
;;-sig: (filename [default])
;;
(define (filename->content-type filename . default)
  (or (and-let* ((cut (string-rindex filename #\.))
                 (mt (mime-types<-extension (subs filename (1+ cut)))))
        (symbol->string (if (pair? mt)
                            (car mt)
                            mt)))
      ;; use ‘if’ here to allow #f for ‘default’
      (if (not (null? default))
          (car default)
          "application/octet-stream")))

;;; (www server-utils filesystem) ends here
