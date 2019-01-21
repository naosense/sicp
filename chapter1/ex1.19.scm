#lang sicp

(#%require rackunit)
(#%require "text.scm")

;; 假设(a0,b0),(a1,b1),(a2,b2)分别是Tpq变换0次1次2次的结果
;; 根据递推式子
;; a <- bq + aq + ap, b <- bp + aq
;; 可以得到
;; a2 = b0(2pq + q^2) + a0(p2 + 2pq + 2q^2), b2 = b0(p^2 + q^2) + a0(2pq + q^2)
;; 从而得出p' = p^2 + q^2, q' = 2pq + q^2
(define (fib-log n)
  (define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q))
                   (+ (* 2 p q) (square q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))
  (fib-iter 1 0 0 1 n))

(check-eq? (fib 0) (fib-log 0))
(check-eq? (fib 1) (fib-log 1))
(check-eq? (fib 2) (fib-log 2))
(check-eq? (fib 11) (fib-log 11))
(check-eq? (fib 8) (fib-log 8))



