#lang sicp

(#%require "text.scm")

(define (stream-of-list lst)
  (if (null? lst)
      the-empty-stream
      (cons-stream (car lst) (stream-of-list (cdr lst)))))

(define input '(1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4))

(define sense-data (stream-of-list input))

;; (display-stream sense-data)

(define (sign-change-detector input last-value)
  (cond ((and (< input 0)
              (>= last-value 0))
         1)
        ((and (>= input 0)
              (< last-value 0))
         -1)
        (else 0)))

(define zero-crossing
  (stream-map sign-change-detector sense-data (cons-stream 0 sense-data)))

(display-stream-n zero-crossing (- (length input) 1))
