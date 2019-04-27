#lang sicp

(define (solve-2nd f y0 dy0 dy/dt)
  (define y (integral (delay dy) y0 dy/dt))
  (define dy (integral (delay ddy) dy0 dy/dt))
  (define ddy (stream-map f dy y))
  y)
