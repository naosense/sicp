#lang sicp

(#%require rackunit)
(#%require "text.scm")

(#%provide (all-defined))

(define (double x)
  (+ x x))

(define (halve x)
  (/ x 2))

(define (fast-multi b n)
  (cond ((= n 0) 0)
        ((even? n) (double (fast-multi b (halve n))))
        (else (+ b (fast-multi b (- n 1))))))

(check-eq? (fast-multi 3 0) 0)
(check-eq? (fast-multi 3 1) 3)
(check-eq? (fast-multi 1 1) 1)
(check-eq? (fast-multi 1 12) 12)
(check-eq? (fast-multi 30 40) 1200)