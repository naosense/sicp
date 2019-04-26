#lang sicp

(#%require rackunit)

(define (pow x n)
  (if (= n 0)
      1
      (* x (pow x (- n 1)))))

;; 2和3是互素的
(define (cons a b)
  (* (pow 2 a) (pow 3 b)))

(define (car c)
  (if (= (remainder c 2) 0)
      (+ 1 (car (/ c 2)))
      0))

(define (cdr c)
  (if (= (remainder c 3) 0)
      (+ 1 (cdr (/ c 3)))
      0))

(check-eqv? (car (cons 0 0)) 0)
(check-eqv? (cdr (cons 0 0)) 0)
(check-eqv? (car (cons 1 0)) 1)
(check-eqv? (cdr (cons 1 0)) 0)
(check-eqv? (car (cons 0 1)) 0)
(check-eqv? (cdr (cons 0 1)) 1)
(check-eqv? (car (cons 2 5)) 2)
(check-eqv? (cdr (cons 2 5)) 5)
      