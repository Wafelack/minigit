(use-modules (web server)
             (web request)
             (web response)
             (web uri))

(define style "body {
	color: #444;
	background-color: #eee;
	margin: 40px auto;
  display: flex;
  flex-direction: column;
  justify-content: center;
	max-width: 1050px;
	line-height: 1.4;
	font-size: 14px;
	font-family: sans-serif;
	padding: 0 10px;
}
")

(define (path-components req)
  (split-and-decode-uri-path (uri-path (request-uri req))))
(define (make-page title body)
  (call-with-output-string 
    (lambda (out)
      (format out "<!DOCTYPE html>
<html lang=en>
  <head>
    <meta charset=utf8 />
    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\" />
    <title>~a</title>
  </head>
  <style>
  ~a
  </style>
  <body>
    <h1>~a</h1>
    ~a
  </body>
</html>" title style title body))))

(define (make-repo-page title repo body)
  (make-page title
             (call-with-output-string
               (lambda (out) (format out
                                     "<table style=\"padding-bottom: 2em;\"><tbody><tr><td><a href=\"/~a\">Log</a></td><td><a href=\"/~a/files\">Files</a></td></tr></tbody></table>
                                     ~a"
                                     repo
                                     repo
                                     body)))))

(define (repo-list repos)
  (values '((content-type . (text/html)))
          (make-page "Minigit Index" 
                     (call-with-output-string
                       (lambda (out) 
                         (format out "~{~a~%~}" 
                                 (map 
                                   (lambda (repo) 
                                     (call-with-output-string (lambda (out) (format out "<li><a href=\"/~a\">~a</a></li>" repo repo))))  
                                   repos)))))))
(define (repo-log repo folder)
  (let ((commits (list-commits
                   (call-with-output-string
                     (lambda (out) (format out "~a/~a" folder repo))))))
    (if (null? commits)
      (not-found repo)
      (values '((content-type . (text/html)))
              (make-repo-page repo repo
                         (call-with-output-string
                           (lambda (out)
                             (format out "<table><thead><tr>
                                     <td><b>Date</b></td>
                                     <td><b>Summary</b></td>
                                     <td><b>Author</b></td>
                                     </thead><tbody>
                                     ~{~a~%~}
                                     </tbody></table>\n"
                                     (map 
                                       (lambda (commit)
                                         (let ((hash (car commit))
                                               (author (car (cdr commit)))
                                               (summary (car (cdr (cdr commit))))
                                               (time (car (cdr (cdr (cdr commit))))))
                                           (call-with-output-string
                                             (lambda (out)
                                               (format out 
                                                       "<tr><td style=\"padding-right: 2em;\">~a</td><td style=\"padding-right: 2em;\"><a href=\"./~a/~a\">~a</a></td><td>~a</td></tr>\n"
                                                       (strftime "%Y-%m-%d %H:%M" (localtime time))
                                                       repo
                                                       hash
                                                       summary
                                                       author)))))
                                       commits)))))))))
(define (not-found repo)
  (values '((content-type . (text/html)))
          (make-page "Repository not found"
                     (call-with-output-string
                       (lambda (out) (format out "Repository ~a does not exist." repo))))))

(define (make-request-handler repos folder)
  (lambda (request body)
    (let ((path (path-components request)))
      (if (null? path)
        (repo-list repos)
        (let ((repo (string->symbol (car path))))
          (if (member repo repos)
            (repo-log repo folder)
            (not-found repo)))))))
