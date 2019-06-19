#lang sicp

(#%require rackunit)

(define (reverse l)
  (if (null? l)
      l
      (append (reverse (cdr l)) (list (car l)))))


(check-equal? (reverse '(1 2 3 4)) '(4 3 2 1))
