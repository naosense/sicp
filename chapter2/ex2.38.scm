#lang sicp

(#%require rackunit)
(#%require "text.scm")

(#%provide (all-defined))

;; fold-right 先进行的是序列的最右边的元素与初始值的运算，然后依次向左
;; fold-left 先进行的是初始值与序列最左边的元素的运算，然后以此向右
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (fold-right op initial sequence)
  (accumulate op initial sequence))

;; (1/(2/(3/1)))
(check-eqv? (fold-right / 1 '(1 2 3)) (/ 3 2))
;; (((1/1)/2)/3)
(check-eqv? (fold-left / 1 '(1 2 3)) (/ 1 6))
;; (list 1 (list 2 (list 3 ())))
(check-equal? (fold-right list nil '(1 2 3)) '(1 (2 (3 ()))))
;; (list (list (list () 1) 2) 3) 
(check-equal? (fold-left list nil '(1 2 3)) '(((() 1) 2) 3))
              