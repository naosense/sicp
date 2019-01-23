#lang sicp

(#%require rackunit)
(#%require "ex1.37.scm")

(define (e-val-rc k)
  (+ 2 (cont-frac-rc (lambda (i) 1.0)
                     (lambda (i)
                       (let ((r (remainder i 3))
                             (q (quotient i 3)))
                         (cond ((= r 0) 1)
                               ((= r 1) 1)
                               ((= r 2) (* (+ q 1) 2)))))
                     k)))

(define (e-val-it k)
  (+ 2 (cont-frac-it (lambda (i) 1.0)
                     (lambda (i)
                       (let ((r (remainder i 3))
                             (q (quotient i 3)))
                         (cond ((= r 0) 1)
                               ((= r 1) 1)
                               ((= r 2) (* (+ q 1) 2)))))
                     k)))

(check-eqv? (e-val-rc 1000) (e-val-it 1000))
(check-eqv? (e-val-rc 100) (e-val-it 100))

