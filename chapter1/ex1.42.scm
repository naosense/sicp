#lang sicp

(#%require rackunit)
(#%require "text.scm")

(#%provide (all-defined))

(define (compose f g)
  (lambda (x)
    (f (g x))))

(check-eqv? ((compose square inc) 6) 49)