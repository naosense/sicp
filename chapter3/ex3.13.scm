#lang sicp

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define z (make-cycle '(a b c)))
z
(last-pair z)

;; [a,]--->[b,]--->[c,]-
;; ^                   |
;; |___________________|