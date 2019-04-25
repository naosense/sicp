#lang sicp

(#%require "text.scm")

(define (stream-limit s tolerance)
  (let ((curr (stream-ref s 0))
        (next (stream-ref s 1)))
    (if (< (abs (- next curr)) tolerance)
        next
        (stream-limit (stream-cdr s) tolerance))))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(sqrt 2 0.1)
