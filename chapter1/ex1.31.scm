#lang sicp

(#%require rackunit)
(#%require "text.scm")

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))


(define (factorial n)
  (product identity 1 inc n))

(check-eq? (factorial 5) 120)
(check-eq? (factorial 1) 1)
(check-eq? (factorial 2) 2)

(define (pi-val n)
  (define (f x)
    (if (even? x)
        (/ (+ x 2) (+ x 1))
        (/ (+ x 1) (+ x 2))))
  (* 4.0 (product f 1 inc n)))
  
           

(pi-val 100)
(pi-val 1000)

;; 迭代版本参考1.30
  