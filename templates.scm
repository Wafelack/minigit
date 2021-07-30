(use-modules (sxml simple))

(define style "style.css") ;; TODO: Replace with config value.

(define (read-style style)
  (with-input-from-file 
      style
      (lambda () 
        (let next ((l '()) (c (read-char)))
          (if (eof-object? c)
            (list->string (reverse l))
            (next (cons c l) (read-char)))))))

(define (make-page title body)
  (string-append 
    (call-with-output-string
    (lambda (out)
      (sxml->xml `((html (@ (lang "en"))
         (head
           (meta (@ (charset "utf8")))
           (meta (@ (name "viewport")
                    (content "width=device-width, initial-scale=1.0")))
           (link (@ (rel "stylesheet")
                    (type "text/css")
                    (href "./style.css")))
           (title ,title))
         (body
           (h1 ,title)
           ,@body))) out)))
    (call-with-output-string
      (lambda (out) (format out "<style>~a</style>" (read-style style))))))
(define (make-repo-page title repo body)
  (make-page title 
             `((table (@ (class "links"))
                            (tbody 
                              (tr
                                (td
                                  (a (@ (href ,(string-append "/" (symbol->string repo))))
                                     Log))
                                (td
                                  (a (@ (href ,(string-append "/" (string-append (symbol->string repo) "/files"))))
                                     Files)))))
                     ,@body)))
