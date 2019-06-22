(define (parse-word word-list)
  (require (not (null? (cdr word-list))))
  (amb
    (car (cdr word-list))
    (parse-word (cons (car word-list)
                      (cdr (cdr word-list))))))
