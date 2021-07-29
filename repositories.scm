(use-modules (git))

(define (get-repos root)
  (let get-repos ((folders (scandir root)))
    (if (null? folders)
      '()
      (let* ((folder (car folders)) 
             (sym (string->symbol folder)) 
             (next (get-repos (cdr folders))))
        (if (and 
          (not (eq? sym '.)) 
          (not (eq? sym '..))
          (access? 
            (call-with-output-string 
              (lambda (out) (format out "~a/~a/.git" root folder))) 
            R_OK))
              (cons (string->symbol folder) next)
              next)))))

(define (list-commits repository)
  (if (not (access?
             (call-with-output-string
               (lambda (out) (format out "~a/.git" repository)))
             R_OK))
    '()
    (let* ((repository (repository-open repository))
            (oid (reference-target (repository-head repository)))
            (first (commit-lookup repository oid)))
      (let get-commit-info ((commit first))
          (let ((author (commit-author commit)))
            (cons 
              (list (oid->string (commit-id commit)) 
                (signature-name author)
                (commit-summary commit)
                (time-time (signature-when author)))
              (if (eq? (commit-parentcount commit) 0) 
                '()
                (get-commit-info (commit-parent commit)))))))))
