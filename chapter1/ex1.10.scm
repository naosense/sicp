#lang sicp

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))


;; 2 ^ 10
(A 1 10)

;; 2 ^ (2 ^ (2 ^ (2 ^ 2)))
(A 2 4)
;; 同上
(A 3 3)

;; 2 * n
(define (f n) (A 0 n))
;; 2 ^ n
(define (g n) (A 1 n))
;; 2 ^ (2 ^ (2 ^ 2)) n次，即(h 1)=2,(h 2)=2^2,(h 3)=2 ^ (2 ^ 2)
(define (h n) (A 2 n))