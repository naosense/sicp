#lang sicp

(#%require rackunit)
(#%require "text.scm")

(define (unique-pair n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (list i j))
          (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pair n))))

(display (unique-pair 3))
(newline)
(display (prime-sum-pairs 3))