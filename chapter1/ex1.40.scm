#lang sicp

(#%require rackunit)
(#%require "text.scm")

(define (cubic a b c)
  (lambda (x)
    (+ (cube x) (* a (square x)) (* b x) c)))

(newtons-method (cubic 1 2 8) 1)
(newtons-method (cubic 0 2 8) 1)
(newtons-method (cubic 0 0 8) 1)