#lang sicp

(define (or-gate a1 a2 output)
  (let ((c1 (make-wire))
        (c2 (make-wire))
        (c3 (make-wire)))
    (inverter a1 c1)
    (inverter a1 c2)
    (and-gate c1 c2 c3)
    (inverter c3 output)
    'ok))