(use-modules (ice-9 ftw))
(use-modules (git))

(load "repositories.scm")
(load "server.scm")
(load "config.scm")

(define repos (get-repos folder))
#;(map display (get-entries
       (let* ((repository (repository-open "../kernel"))
              (oid (reference-target (repository-head repository)))
              (head (commit-lookup repository oid)))
         head)))

(run-server (make-request-handler repos folder) 'http `(#:port ,port))
