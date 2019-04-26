#lang sicp

(#%require rackunit)
(#%require "text.scm")

;; 1.7
;; 应该是数比较小的时候不能工作,是不是数比较大的时候也不行,会溢出?不过
;; 我实验了20位的数值也可以。
;; 只需要重新定义一个good-enough函数即可
(define (good-enough2? guess x)
  (< (abs (/ (- guess (improve guess x))
             guess))
     0.001))

(define (sqrt-iter2 guess x)
  (if (good-enough2? guess x)
      guess
      (sqrt-iter2 (improve guess x)
                 x)))

(define (sqrt2 x)
  (sqrt-iter2 1.0 x))

(sqrt 0.01)
(sqrt2 0.01)
(sqrt 0.0001)
(sqrt2 0.0001)