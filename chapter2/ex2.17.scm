#lang sicp

(#%require rackunit)
(#%require "text.scm")

(define (last-pair l)
  (if (= (length l) 1)
      l
      (last-pair (cdr l))))

(check-equal? (last-pair (list 1 2 3 4)) '(4))
(check-equal? (last-pair (list 4)) '(4))
  