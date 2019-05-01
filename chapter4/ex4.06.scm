#lang sicp

(define (let? exp) (tagged-list? exp 'let))

(define (let-locals exp) (cadr exp))

(define (let-variable locals)
  (if (null? locals)
      '()
      (cons (caar locals) (let-variable (cdr locals)))))

(define (let-value locals)
  (if (null? locals)
      '()
      (cons (cadar locals) (let-value (cdr locals)))))

(display (let-variable '((a 1) (b 2))))
(display (let-value '((a 1) (b 2))))

(define (let-body exp) (cddr exp))

(define (let->combination exp)
  (cons (make-lambda (let-variable (let-locals exp))
                     (let-body exp))
        (let-value (let-locals exp))))

(define (make-let locals body)
  (cons 'let (cons locals body)))
