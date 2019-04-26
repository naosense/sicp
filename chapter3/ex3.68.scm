#lang sicp

(#%require "text.scm")

(define (pairs s t)
    (interleave
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t))))

;; 将会导致死循环，因为递归函数有两个要素：
;; 1: 初始值 2: 对参数做出改变，使之向初始值条件靠拢
;; 有了延迟计算后2可以省略，但是1不能少
(display-stream-n (pairs integers integers) 10)
