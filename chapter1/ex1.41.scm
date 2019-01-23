#lang sicp

(#%require rackunit)
(#%require "text.scm")

(define (double f)
  (lambda (x) (f (f x))))

(check-eqv? ((double inc) 3) 5)
(check-eqv? ((double square) 3) 81)
;; (double double)
;; (lambda (x) (double (double x)))
;; (lambda (x) ((lambda (x1) (double (double x1))) ((lambda (x2) (double (double x2))) x)))
;; x为inc，代入得
;; (lambda (x) ((lambda (x1) (double (double x1))) ((lambda (x2) (double (double x2))) x)))
;; ((lambda (x1) (double (double x1))) ((lambda (x2) (double (double x2))) inc))
;; ((lambda (x1) (double (double x1))) (double (double inc)))
;; (double (double (double (double inc))))
;; (double (double (double 加2)))
;; (double (double 加4))
;; (double 加8)
;; 加16
(((double (double double)) inc) 5)

