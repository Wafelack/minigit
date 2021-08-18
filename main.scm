(use-modules (ice-9 ftw))
(use-modules (git))
(use-modules (rnrs bytevectors))

(load "repositories.scm")
(load "server.scm")
(load "config.scm")

(define *repo* (repository-open "../kernel"))

(define repos (get-repos folder))
(map (lambda (entry) 
       (let ((name (car entry))
             (id (cdr entry)))
       (display name)
       (newline)
       (let ((blob (blob-lookup *repo* id)))
        (display blob)
        (newline)
        (display (utf8->string (blob-content blob)))
        (newline))))
     (get-entries
       (let* ((oid (reference-target (repository-head *repo*)))
              (head (commit-lookup *repo* oid)))
         head)))

(run-server (make-request-handler repos folder) 'http `(#:port ,port))
