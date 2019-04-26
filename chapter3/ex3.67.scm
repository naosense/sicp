#lang sicp

(#%require "text.scm")

(define (pairs-matrix s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (interleave
        (stream-map (lambda (x) (list x (stream-car s)))
                    (stream-cdr t))
        (stream-map (lambda (x) (list (stream-car s) x))
                    (stream-cdr t)))
      (pairs-matrix (stream-cdr s) (stream-cdr t)))))

(display-stream-n (pairs-matrix integers integers) 20)
