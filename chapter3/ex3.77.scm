#lang sicp

(define (integral delayed-integrand initial-value dt)
  (cons-stream initial-value
               (let ((integrand (force delayed-integrand)))
                 (if (stream-null? integrand)
                     the-empty-stream
                     (+ (* dt (stream-car integrand))
                        initial-value)
                     dt))))
