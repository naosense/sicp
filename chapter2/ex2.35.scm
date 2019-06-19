#lang sicp

(#%require rackunit)

(define (count-leaves t)
  (accumulate + 0 (map (lambda (sub)
                         (if (pair? sub)
                             (count-leaves sub)
                             1))
                       t)))
;; common
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


(check-eqv? (count-leaves '(1 (2 (3 4) 5))) 5)
