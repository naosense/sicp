#lang sicp

(#%require "text.scm")

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define factorials
  (cons-stream 1 (mul-streams (stream-cdr integers) factorials)))

(display-stream-n factorials 10)