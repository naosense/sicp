#lang sicp

(#%require "lazy.scm")

(define (let? exp) (tagged-list? exp 'let))

(define (let-locals exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (cadr exp)))

(define (let-variable locals)
  (if (null? locals)
      '()
      (cons (caar locals) (let-variable (cdr locals)))))

(define (let-value locals)
  (if (null? locals)
      '()
      (cons (cadar locals) (let-value (cdr locals)))))

(define (let-body exp)
  (if (symbol? (cadr exp))
      (cdddr exp)
      (cddr exp)))

(define (let->combination exp)
  (let ((f (cadr exp))
        (locals (let-locals exp))
        (body (let-body exp)))
    (if (symbol? f)
        (make-begin (list (list 'define f (make-lambda (let-variable locals) body))
                          (cons f (let-value locals))))
        (cons (make-lambda (let-variable locals) body)
              (let-value locals)))))

(define (make-let locals body)
  (cons 'let (cons locals body)))

(display (let->combination '(let fib-iter ((a 1)
                                           (b 0)
                                           (count n))
                              (if (= count 0)
                                  b
                                  (fib-iter (+ a b) a (- count 1))))))
