(use-modules (web server)
             (web request)
             (web response)
             (web uri))
(use-modules (sxml simple))
(use-modules (git))

(load "templates.scm")

(define (path-components req)
  (split-and-decode-uri-path (uri-path (request-uri req))))

(define (repo-list repos)
  (values '((content-type . (text/html)))
            (make-page "Minigit Index"
                       (map 
                         (lambda (repo)
                           `((li
                             (a (@ (href ,(string-append "/" (symbol->string repo))))
                               ,repo)))) 
                         repos))))
(define (repo-log repo folder)
  (let ((commits (list-commits
                   (call-with-output-string
                     (lambda (out) (format out "~a/~a" folder repo))))))
    (if (null? commits)
      (not-found)
      (values '((content-type . (text/html)))
                (make-repo-page repo repo
                           `((table
                             (thead
                               (tr 
                                 (td (b Date)) 
                                 (td (b Summary)) 
                                 (td (b Author))))
                             (tbody
                               ,(map
                                  (lambda (commit)
                                    (let ((hash (list-ref commit 0))
                                          (author (list-ref commit 1))
                                          (summary (list-ref commit 2))
                                          (time (list-ref commit 3)))
                                      `(tr
                                         (td (@ (class date-summary))
                                             ,(strftime "%Y-%m-%d %H:%M" (localtime time)))
                                         (td (@ (class date-summary))
                                             (a (@ (href ,(call-with-output-string
                                                            (lambda (out) 
                                                              (format out
                                                                      "/~a/~a" repo hash)))))
                                                ,summary))
                                         (td ,author))))
                                  commits)))))))))
(define (not-found)
  (values '((content-type . (text/html)))
            (make-page "Not Found." `((center (h1 "Error 404: Not Found"))))))

(define (file-view repo folder path)
  (not-found)) ;; TODO

(define (repo-tree repo folder)
  (define repo_path (call-with-output-string
                      (lambda (out) (format out "~a/~a" folder repo))))
  (if (not (access? repo_path R_OK))
    (not-found)
    (let* ((repository (repository-open repo_path))
           (oid (reference-target (repository-head repository)))
           (head (commit-lookup repository oid)))
      (values '((content-type . (text/html)))
              (make-repo-page repo repo 
                      `((table
                          (thead
                            (tr
                              (td (b File))
                              (td (b Size))))
                          (tbody
                            ,(map (lambda (entry)
                                (let* ((name (car entry))
                                      (id (cdr entry))
                                      (blob (blob-lookup repository id))
                                      (size (blob-size blob)))
                                  `(tr
                                     (td (@ (class date-summary))
                                         (a (@ (href ,(call-with-output-string
                                                        (lambda (out)
                                                          (format out "/~a/files/~a" repo name))))) ,name))
                                     (td ,size))))
                           (get-entries head))))))))))

(define (repo-files repo folder path)
  (if (null? path)
    (repo-tree repo folder)
    (file-view repo folder path)))

(define (route-tab repo folder name args)
  (if (null? name)
    (repo-log repo folder)
    (case (string->symbol (car name))
      ('files (repo-files repo folder args))
      (else (not-found)))))

(define (make-request-handler repos folder)
  (lambda (request body)
    (let ((path (path-components request)))
      (if (null? path)
        (repo-list repos)
        (let ((repo (string->symbol (car path))) (next (cdr path)))
          (if (member repo repos)
            (cond
              ((> 0 (length next)) (route-tab repo folder (string->symbol (car next)) (cdr next)))
              (else (route-tab repo folder next '())))
            (not-found)))))))
