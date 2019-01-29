#lang sicp

(#%require rackunit)
(#%require "text.scm")

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (inc y)) 0 sequence))


(check-equal? (map inc '(1 2 3)) '(2 3 4))
(check-equal? (append '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))
(check-eqv? (length '(1 2 3 4 5 6)) 6)