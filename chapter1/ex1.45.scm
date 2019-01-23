#lang sicp

(#%require rackunit)
(#%require "text.scm")
(#%require "ex1.43.scm")

(define (pow x n)
  (if (= n 0)
      1
      (* x (pow x (- n 1)))))

;; 本题这个次数纯粹胡蒙乱猜，没有找到相关的数学知识
;; 也没有相关的技术储备，暂时忽略
;; 参考了网上答案(floor (log2 n))，测试好像不对啊
(define (log2 x) (/ (log x) (log 2)))

(define (nth-root x n)
  (fixed-point (repeated (average-damp
                          (lambda (y)
                            (/ x (pow y (- n 1)))))
                         (* 3 n))
               1.0))

(nth-root 4 2)
(nth-root 4 3)
(nth-root 4 4)
(nth-root 4 5)
(nth-root 4 6)
(nth-root 4 7)
(nth-root 5 32)
