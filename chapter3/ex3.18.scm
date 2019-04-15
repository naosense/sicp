#lang sicp

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define z (make-cycle '(a b c)))

(define (cycle? x) 
  (let ((visited '())) 
    (define (iter x) 
      (set! visited (cons x visited)) 
      (cond ((not (pair? x)) false)
            ((null? (cdr x)) false) 
            ((memq (cdr x) visited) true) 
            (else (iter (cdr x))))) 
    (iter x))) 

(cycle? z)
(cycle? '())
(cycle? '(a))
(cycle? (make-cycle '(a)))
(cycle? '(a b c))