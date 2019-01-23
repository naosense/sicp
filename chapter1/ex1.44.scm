#lang sicp

(#%require rackunit)
(#%require "text.scm")
(#%require "ex1.43.scm")

(define (smooth f)
  (define dx 0.00001)
  (lambda (x)
    (average (f (- x dx)) (f (+ x dx)))))

((smooth square) 2)
(((repeated smooth 10) square) 2)