#lang sicp

(#%require "text.scm")

(define (integral-simpson f a b n)
  (define h (/ (- b a) n))
  (define (add-2h x) (+ x h h))
  (* (+ (f a)
        (* 4 (sum f (+ a h) add-2h b))
        (* 2 (sum f (add-2h a) add-2h b)))
     (/ h 3.0)))

(integral-simpson cube 0 1 1000)
(integral-simpson cube 0 1 100)
(integral cube 0 1 0.01)
(integral cube 0 1 0.001)
  
  