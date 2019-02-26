#lang sicp
;; x 2
;; set (1 3 4)
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

(display (adjoin-set 2 '(1 3 4)))