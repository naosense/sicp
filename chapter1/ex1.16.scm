#lang sicp

(#%require rackunit)
(#%require "text.scm")


;; 当n为偶数
;; 根据恒等式b ^ n = (b ^ 2) ^ (n / 2)
;; 迭代a = a，b = b ^ 2，n = n / 2
;; 当n为奇数
;; 根据恒等式b ^ n = b * b ^ (n - 1)，n - 1为偶数
;; 迭代a = a * b，b = b，n = n - 1
(define (fast-expt-i b n)
  (define (fast-expt-iter a b count)
    (cond ((= count 0) a)
          ((even? count) (fast-expt-iter a (square b) (/ count 2)))
          (else (fast-expt-iter (* a b) b (- count 1)))))
  (fast-expt-iter 1 b n))

(check-eq? (fast-expt 3 0) (fast-expt-i 3 0))
(check-eq? (fast-expt 3 3) (fast-expt-i 3 3))
(check-eq? (fast-expt 3 6) (fast-expt-i 3 6))
(check-eq? (fast-expt 2 32) (fast-expt-i 2 32))
