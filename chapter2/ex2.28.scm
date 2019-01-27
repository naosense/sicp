#lang sicp

(#%require rackunit)

(define (fringe l)
  (cond ((null? l) nil)
        ((not (pair? (car l)))
         (append (list (car l)) (fringe (cdr l))))
        (else
         (append (fringe (car l)) (fringe (cdr l))))))

(check-equal? (fringe '((1 2) (3 4))) '(1 2 3 4))
(check-equal? (fringe '((1 2) 5 (6 7) (3 4))) '(1 2 5 6 7 3 4))
(check-equal? (fringe '((1 2) (5 (6 7)) (3 4))) '(1 2 5 6 7 3 4))
        