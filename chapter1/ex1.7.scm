#lang sicp

;; 1.7
;; 应该是数比较小的时候不能工作,是不是数比较大的时候也不行,会溢出?不过
;; 我实验了20位的数值也可以。
;; 只需要重新定义一个good-enough函数即可
(define (good-enough2? guess x)
  (< (abs (/ (- guess (improve guess x)) guess)) 0.001))