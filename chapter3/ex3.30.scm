#lang sicp

(define (ripple-carry-adder A B S c)
  (let ((cout (make-wire)))
    (cond ((null? A) c)
          (else
           (full-adder (car A) (car B) c (car S) cout)
           (ripple-carry-adder (cdr A) (cdr B) (cdr S) cout)))))
