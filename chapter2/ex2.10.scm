#lang sicp

(#%require "text.scm")

(define (div-interval x y)
  (if (< (* (upper-bound y) (lower-bound y)) 0)
      (error "interval contains 0")
      (mul-interval x
                    (make-interval (/ 1 (upper-bound y))
                                   (/ 1 (lower-bound y))))))

(define iv1 (make-interval -1 2))
(define iv2 (make-interval 3 7))
(div-interval iv1 iv2)
(div-interval iv2 iv1)