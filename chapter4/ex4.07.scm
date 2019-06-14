#lang sicp

(#%require "lazy.scm")

(define (let*? exp) (tagged-list? exp 'let*))

(define (let*-locals exp) (cadr exp))

(define (let*-body exp) (cddr exp))

(define (let*->nested-lets exp)
  (define (iter locals body)
    (if (null? locals)
        body
        (list (make-let (list (car locals))
                        (iter (cdr locals) body)))))
  (car (iter (let*-locals exp) (let*-body exp))))

(display (let*->nested-lets '(let* ((x 3)
                                    (y (+ x 2))
                                    (z (+ x y 5)))
                               (* x z))))
