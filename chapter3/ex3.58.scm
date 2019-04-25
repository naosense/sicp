#lang sicp

(#%require "text.scm")

(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

;; 1 4 2 8 5 7...
(display-stream-n (expand 1 7 10) 20)

;; 3 7 5 0 0 0...
(display-stream-n (expand 3 8 10) 20)