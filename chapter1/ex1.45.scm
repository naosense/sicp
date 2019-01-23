#lang sicp

(#%require rackunit)
(#%require "text.scm")
(#%require "ex1.43.scm")

(define (pow x n)
  (if (= n 0)
      1
      (* x (pow x (- n 1)))))

(define (log2 x) (/ (log x) (log 2)))

(define (nth-root x n)
  (fixed-point (repeated (average-damp
                          (lambda (y)
                            (/ x (pow y (- n 1)))))
                         (floor (log2 n)))
               1.0))

;(nth-root 4 2)
;(nth-root 4 3)
;(nth-root 4 4)
;(nth-root 4 5)
;(nth-root 4 6)
(nth-root 5 32)
  