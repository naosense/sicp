#lang sicp

;; a)
;; 这样将会导致定义和赋值被识别为过程应用，从而导致运算错误

;; b)
(define (application? exp) (tagged-list? exp 'call))

(define (operator exp) (cadr exp))

(define (operands exp) (cddr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))
