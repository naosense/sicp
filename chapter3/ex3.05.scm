#lang sicp

(#%require "text.scm")

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (square x) (* x x))

(define (estimate-integral p x1 x2 y1 y2 trials)
  (abs (* (- x1 x2) (- y1 y2) (monte-carlo trials p) 1.0)))

(define p
    (lambda ()
      (<= (+ (square (- (random-in-range -1.0 1.0) 0))
             (square (- (random-in-range -1.0 1.0) 0)))
          1)))

(estimate-integral p -1 1 -1 1 1000000)
