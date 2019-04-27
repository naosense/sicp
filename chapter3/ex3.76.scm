#lang sicp

(#%require "text.scm")

(define (smooth stream)
  (let ((s0 (stream-ref stream 0))
        (s1 (stream-ref stream 1)))
    (cons-stream (/ (+ s0 s1) 2)
                 (smooth (stream-cdr stream)))))

(display-stream-n (smooth integers) 10)
