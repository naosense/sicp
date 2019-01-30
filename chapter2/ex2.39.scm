#lang sicp

(#%require rackunit)
(#%require "ex2.38.scm")

(define (reverse-1 sequence)
  ;; x: car
  ;; y: result
  ;; (append result (list car))
  (fold-right (lambda (x y) (append y (list x))) nil sequence))

(define (reverse-2 sequence)
  ;; x: result
  ;; y: car
  ;; (append (list car) result)
  (fold-left (lambda (x y) (append (list y) x)) nil sequence))


(check-equal? (reverse-1 '(1 2 3)) (reverse-2 '(1 2 3)))