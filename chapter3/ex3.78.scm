#lang sicp

(define (solve-2nd a b y0 dy0 dy/dt)
  (define y (integral (delay dy) y0 dy/dt))
  (define dy (integral (delay ddy) dy0 dy/dt))
  (define ddy (add-streams (scale-stream dy a)
                           (scale-stream y b)))
  y)
