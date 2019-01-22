#lang sicp

(#%require rackunit)
(#%require "text.scm")

;; 递归版本
;(define (accumulate combiner null-value term a next b)
;  (if (> a b)
;      null-value
;      (combiner (term a)
;                (accumulate combiner null-value term (next a) next b))))

;; 迭代版本
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

;; another sum
(define (sum-a term a next b)
  (accumulate + 0 term a next b))

(define (product-a term a next b)
  (accumulate * 1 term a next b))

(define (sum-integers-a a b)
  (sum-a identity a inc b))

(define (factorial-a n)
  (product-a identity 1 inc n))

(check-eq? (sum-integers 1 10) (sum-integers-a 1 10))
(check-eq? (sum-integers 1 100) (sum-integers-a 1 100))

(check-eq? (factorial-a 5) 120)
(check-eq? (factorial-a 1) 1)
(check-eq? (factorial-a 2) 2)