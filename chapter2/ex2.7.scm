#lang sicp

(#%require "text.scm")

(define iv1 (make-interval -1 2))
(define iv2 (make-interval 3 7))

(add-interval iv1 iv2)
(mul-interval iv1 iv2)
(div-interval iv1 iv2)