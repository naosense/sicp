#lang sicp

(#%require "text.scm")

(define (f g)
  (g 2))

(f square)
(f (lambda (z) (* z (+ z 1))))
;; (f f)
;; (f 2)
;; (2 2)
;; 报错，2不是一个过程

