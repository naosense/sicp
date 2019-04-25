#lang sicp

(#%require "text.scm")

(define (partial-sums s)
  (let ((s0 (stream-car s))
        (s1 (stream-cdr s)))
    (define accu
      (cons-stream s0 (add-streams s1 accu)))
    accu))
                 

(display-stream-n (partial-sums double) 10)