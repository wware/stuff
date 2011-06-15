;;-*-mode:lisp-*-
;; $Id: find-files.scm,v 1.2 1995/12/06 14:13:16 gjc Exp $

(define (list-files dir)
  (let ((s (opendir dir))
	(result nil)
	(f nil))
    (while (set! f (readdir s))
      (set! result (cons f result)))
    (closedir s)
    (nreverse result)))

(define (find-files start)
  (let ((l (list-files start))
	(result nil)
	(f nil)
	(s nil))
    (while l
      (if (not (equal? (car l) ".."))
	  (begin (set! f (string-append start "/" (car l)))
		 (set! s (lstat f))
		 (set! result (cons (cons f s) result))
		 (if (and (memq 'DIR (assq 'mode s))
			  (not (equal? (car l) ".")))
		     (set! result (nconc (nreverse (find-files f)) result)))))
      (set! l (cdr l)))
    (nreverse result)))

(define (get-file-info l)
  ;; used on the result of a find-files.
  (let ((mode (assq 'mode l)))
    (cond ((memq 'LNK mode)
	   (let ((result (*catch 'errobj (readlink (car l)))))
	     (if (string? result)
		 (set-cdr! l
			   (cons (cons 'readlink result)
				 (cdr l))))))
	  ((memq 'REG mode)
	   (let ((f nil)
		 (c (md5-init))
		 (b (cons-array 4096 'string)))
	     (*catch 'errobj
		     (begin (set! f (fopen (car l) "r"))
			    (md5-update c b f)
			    (set-cdr! l
				      (cons (cons 'md5
						  (array->hexstr
						   (md5-final c)))
					    (cdr l)))))
	     (and f (fclose f))))))
  l)

(define (get-directory-snapshot dir)
  (mapcar get-file-info
	  (find-files dir)))

(define (snapshot save-file dir)
  (save-forms save-file (get-directory-snapshot dir)))



  
