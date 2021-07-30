#!/usr/bin/env guile
!#
(use-modules (ice-9 ftw))

(load "repositories.scm")
(load "server.scm")
(load "config.scm")

(define repos (get-repos folder))

(run-server (make-request-handler repos folder) 'http `(#:port ,port))
