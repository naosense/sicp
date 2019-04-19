#lang sicp

(#%require "text.scm")

(define (averager a b c)
  (let ((sum (make-connector))
        (coefficient (make-connector)))
    (constant 2 coefficient)
    (adder a b sum)
    (multiplier c coefficient sum)))

(define a (make-connector))
(define b (make-connector))
(define c (make-connector))
(probe "a" a)
(probe "b" b)
(probe "c" c)
(averager a b c)
(set-value! b 25 'user)
(set-value! c 0 'user) 