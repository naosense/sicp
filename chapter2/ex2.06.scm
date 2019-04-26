#lang sicp

;; 这个题目没搞懂啥意思，暂时略过
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(lambda (f1) (lambda (x1) x1))
(lambda (f) (lambda (x) (f ((n f) x))))

(lambda (f) (lambda (x) (f (((lambda (x1) x1)) x))))

(add-1 zero)