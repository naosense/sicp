#lang sicp

(#%require rackunit)
(#%require "ex1.17.scm")

;; 迭代版本的乘法
(define (multi a b)
  (define (multi-iter acc a b)
    (cond ((= b 0) acc)
          ((even? b) (multi-iter acc (double a) (halve b)))
          (else (multi-iter (+ acc a) a (- b 1)))))
  (multi-iter 0 a b))


(check-eq? (multi 3 0) 0)
(check-eq? (multi 3 1) 3)
                                
  