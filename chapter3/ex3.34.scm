#lang sicp

(#%require "text.scm")

(define (squarer a b)
  (multiplier a a b))

(define a (make-connector))
(define b (make-connector))
(probe "a" a)
(probe "b" b)
(squarer a b)
(set-value! b 3 'user)
;; 这个时候不会打印a