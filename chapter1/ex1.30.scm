#lang sicp

(#%require rackunit)
(#%require "text.scm")

(define (sum-i term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (sum-integers-i a b)
  (sum-i identity a inc b))

(check-eq? (sum-integers 1 10) (sum-integers-i 1 10))
(check-eq? (sum-integers 1 100) (sum-integers-i 1 100))
