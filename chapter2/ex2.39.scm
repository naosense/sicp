#lang sicp

(#%require rackunit)
(#%require "ex2.38.scm")

;(define (reverse sequence)
;  (fold-right (lambda (x y) (append y (list x))) nil sequence))

(define (reverse sequence)
  (fold-left (lambda (x y) (append (list y) x)) nil sequence))
  
(display (reverse '(1 2 3)))