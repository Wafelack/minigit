(use-modules (ice-9 ftw))

(load "repositories.scm")
(load "server.scm")

(define args (cdr (command-line)))
(define folder (if (null? args)
                 "./"
                 (car args)))

(define repos (get-repos folder))

(run-server (make-request-handler repos folder))
