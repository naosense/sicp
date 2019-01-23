#lang sicp

(#%require rackunit)
(#%require "text.scm")
(#%require "ex1.42.scm")

(#%provide (all-defined))

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

(check-eqv? ((repeated square 2) 5) 625)