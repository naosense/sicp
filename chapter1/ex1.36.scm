#lang sicp

(#%require "text.scm")

;; 加入平均阻尼的步数少
(fixed-point (lambda (x) (/ (log 1000) (log x))) 2)
(fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2)