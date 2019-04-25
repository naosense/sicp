#lang sicp

(#%require "text.scm")

(define (ln-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln-summands (+ n 1)))))

(define ln-stream
  (partial-sums (ln-summands 1)))

(display-stream-n ln-stream 10)
(display-stream-n (euler-transform ln-stream) 10)
(display-stream-n (accelerated-sequence euler-transform ln-stream) 10)
