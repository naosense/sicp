#lang sicp

(#%require "ex2.56.scm")

;; a)
(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

;(define (sum? x)
;  (and (pair? x) (eq? (cadr x) '+)))
;
;(define (product? x)
;  (and (pair? x) (eq? (cadr x) '*)))
;
;(define (addend s) (car s))
;
;(define (augend s) (caddr s))
;
;(define (multiplier p) (car p))
;
;(define (multiplicand p) (caddr p))

;; b)
(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(define (before item x)
  (define (iter exp result)
    (cond ((null? exp) false)
          ((eq? item (car exp)) result)
          (else (iter (cdr exp) (append result (list (car exp)))))))
  (iter x '()))

;(display (before '+ '(x * y + z)))

(define (operation expr)
  (if (memq '+ expr)
      '+
      '*))

(define (sum? x)
  (and (pair? x) (eq? (operation x) '+)))

(define (product? x)
  (and (pair? x) (eq? (operation x) '*)))

(define (addend s)
  (let ((bf (before '+ s)))
    (if (= (length bf) 1)
        (car bf)
        bf)))


(define (augend s)
  (let ((af (cdr (memq '+ s))))
    (if (= (length af) 1)
        (car af)
        af)))

(define (multiplier p)
  (let ((bf (before '* p)))
    (if (= (length bf) 1)
        (car bf)
        bf)))

(define (multiplicand p)
  (let ((af (cdr (memq '* p))))
    (if (= (length af) 1)
        (car af)
        af)))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (let ((n (exponent exp))
               (u (base exp)))
           (make-product n
                         (make-product (deriv u var)
                                       (make-exponentiation u (- n 1))))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(display (deriv '(x + (3 * (x + (y + 2)))) 'x))
(display (deriv '(x + 3 * (x + y + 2)) 'x))
