#lang sicp
(#%require rackunit)

(define (max-two x y z)
  (define (min-three x y z)
    (cond ((>= x y) (if (>= y z) z y))
          (else (if (>= x z) z x))))
  (- (+ x y z) (min-three x y z)))


(check-eq? (max-two 2 3 4) 7)
(check-eq? (max-two -1 2 0) 2)
