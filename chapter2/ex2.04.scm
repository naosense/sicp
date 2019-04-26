#lang sicp

(#%require rackunit)

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

(check-eqv? (car (cons 1 2)) 1)
(check-eqv? (cdr (cons 1 2)) 2)
;(check-eqv? (car (car (cons 1 (cons 2 3)))) 2)
(check-eqv? (cdr (cdr (cons 1 (cons 2 3)))) 3)
(check-eqv? (car (cdr (cons 1 (cons 2 3)))) 2)