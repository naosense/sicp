#lang sicp

(#%require rackunit)
(#%require "text.scm")

(define (count-leaves t)
  (accumulate + 0 (map (lambda (sub)
                         (if (pair? sub)
                             (count-leaves sub)
                             1))
                       t)))
                         


(check-eqv? (count-leaves '(1 (2 (3 4) 5))) 5)