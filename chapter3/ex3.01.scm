#lang sicp

(define (make-accumulator init)
  (lambda (val)
    (set! init (+ init val))
    init))

(define A (make-accumulator 5))
(A 10)
(A 10)