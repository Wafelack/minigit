(use-modules (web server)
             (web request)
             (web response)
             (web uri)
             (sxml simple))

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
      (not-found repo)
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
                                    (let ((hash (car commit))
                                          (author (car (cdr commit)))
                                          (summary (car (cdr (cdr commit))))
                                          (time (car (cdr (cdr (cdr commit))))))
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
(define (not-found repo)
  (values '((content-type . (text/html)))
            (make-page "Repository not found" `((p ,"Repository `",repo "` does not exist.")))))

(define (make-request-handler repos folder)
  (lambda (request body)
    (let ((path (path-components request)))
      (if (null? path)
        (repo-list repos)
        (let ((repo (string->symbol (car path))))
          (if (member repo repos)
            (repo-log repo folder)
            (not-found repo)))))))
