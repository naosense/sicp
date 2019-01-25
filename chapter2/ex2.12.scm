#lang sicp

(#%require "text.scm")

(#%provide (all-defined))

(define (make-center-percent c p)
  (let ((r (/ p 100.0)))
    (make-interval (* c (- 1 r))
                   (* c (+ 1 r)))))

(define (percent i)
  (* (/ (- (upper-bound i) (lower-bound i))
        (center i))
     50))

(percent (make-center-percent 35 21))