(use-modules (ice-9 ftw))

(load "repositories.scm")
(load "server.scm")

(define args (cdr (command-line)))
(define folder (if (null? args)
                 "./"
                 (car args)))

(define repos (get-repos folder))

#| (let process-commit ((commits (list-commits "../kernel")))
  (if (not (null? commits))
    (let* ((commit (car commits))
           (hash (car commit))
           (author (car (cdr commit)))
           (summary (car (cdr (cdr commit))))
           (time (car (cdr (cdr (cdr commit))))))
      (begin
        (newline)
        (format #t "HASH: ~a" hash)
        (newline)
        (format #t "AUTHOR: ~a" author)
        (newline)
        (format #t "SUMMARY: '~a'" summary)
        (newline)
        (format #t "TIME: ~a" (strftime "%Y-%m-%d %H:%M" (localtime time)))
        (newline)
        (process-commit (cdr commits)))))) |#

(run-server (make-request-handler repos folder))
