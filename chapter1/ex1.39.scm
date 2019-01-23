#lang sicp

(#%require rackunit)
(#%require "ex1.37.scm")

(define (tan-cf-rc x k) 
  (cont-frac-rc (lambda (i) 
                  (if (= i 1) x (- (* x x)))) 
                (lambda (i) 
                  (- (* i 2) 1)) 
                k))

(define (tan-cf-it x k) 
  (cont-frac-it (lambda (i) 
                  (if (= i 1) x (- (* x x)))) 
                (lambda (i) 
                  (- (* i 2) 1)) 
                k))

(check-eqv? (tan-cf-rc 0.7854 1000) (tan-cf-it 0.7854 1000))
(check-eqv? (tan-cf-rc 0.4712 1000) (tan-cf-it 0.4712 1000))

