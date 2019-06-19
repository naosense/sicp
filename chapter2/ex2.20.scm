#lang sicp

(#%require rackunit)

(define (same-parity first . remains)
  (let ((parity? (if (even? first) even? odd?)))
    (define (iter l)
      (if (null? l)
          l
          (if (parity? (car l))
              (cons (car l) (iter (cdr l)))
              (iter (cdr l)))))
    (cons first (iter remains))))



(check-equal? (same-parity 1 2 3 4 5 6 7) '(1 3 5 7))
(check-equal? (same-parity 2 3 4 5 6 7) '(2 4 6))
