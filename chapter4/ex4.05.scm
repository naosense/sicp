#lang sicp

(define (cond-actions clause)
  (if (memq '=> clause)
      (list (caddr clause) (car clause))
      (cdr clause)))

(display (cond-actions '((assoc b ((a 1) (b 2))) => cadr)))
(display (cond-actions '((assoc b ((a 1) (b 2))) 'false)))
