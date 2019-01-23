#lang sicp

(#%require "text.scm")

(fixed-point cos 1.0)

;; 根据黄金分割的定义x + 1 = x^2
;; 两边除以x得1 + 1/x = x
;; 根据不动点定义设f(x) = 1 + 1/x
;; 从而得f(x) = x,
;; 即黄金分割是f(x) = 1 + 1/x的不动点
(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)