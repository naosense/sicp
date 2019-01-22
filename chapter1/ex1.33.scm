#lang sicp

(#%require rackunit)
(#%require "text.scm")

;; 递归版本
(define (filter-accumulate filter combiner null-value term a next b)
  (cond ((> a b) null-value)
        ((filter a)
         (combiner (term a)
                   (filter-accumulate filter combiner null-value term (next a) next b)))
        (else (filter-accumulate filter combiner null-value term (next a) next b))))

(define (sum-prime a b)
  (filter-accumulate prime? + 0 identity a inc b))

(sum-prime 2 3)
(sum-prime 2 5)
  
(define (product-prime n)
  (define (mutual-prime? x)
    (= (gcd x n) 1))
  (filter-accumulate mutual-prime? * 1 identity 1 inc n))

(product-prime 2)
(product-prime 3)
(product-prime 10)