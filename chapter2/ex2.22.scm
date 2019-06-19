#lang sicp

(#%require rackunit)

(define (square x) (* x x))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (append answer
                      (list (square (car things)))))))
  (iter items nil))

(check-equal? (square-list '(1 2 3)) '(1 4 9))
