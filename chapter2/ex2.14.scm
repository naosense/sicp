#lang sicp

(#%require "text.scm")
(#%require "ex2.12.scm")

(#%provide (all-defined))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(define A (make-center-percent 35 1))
(define B (make-center-percent 100 2))

(div-interval A A)
(div-interval A B)


