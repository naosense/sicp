#lang sicp

(define (for-each proc items)
  (cond ((null? items) (display ""))
        (else (proc (car items))
              (for-each proc (cdr items)))))


(for-each (lambda (x) (newline) (display x))
          '(57 321 88))