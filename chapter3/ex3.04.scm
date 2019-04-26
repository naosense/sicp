#lang sicp

(#%require "ex3.2.scm")

(define (make-account balance password)
  ; 在这里定义和在外面定义对环境的影响
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define warn
    (make-monitored (lambda (msg)
                      (display "Incorrect password")
                      (newline))))
  (define (dispatch pass m)
    (if (eq? password pass)
        (begin
          (warn 'reset-count)
          (cond ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                (else (error "Unknown request -- MAKE-ACCOUNT"
                             m))))
        (if (>= (warn 'how-many-calls) 7)
            ;; call-the-cops
            (error "Cops")
            warn))) 
  dispatch)

(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40)
((acc 'some-password 'deposit) 50)
((acc 'some-password 'deposit) 50)
((acc 'some-password 'deposit) 50)
((acc 'some-password 'deposit) 50)
((acc 'secret-password 'withdraw) 40)
((acc 'some-password 'deposit) 50)
((acc 'some-password 'deposit) 50)
((acc 'some-password 'deposit) 50)
((acc 'some-password 'deposit) 50)
((acc 'some-password 'deposit) 50)
((acc 'some-password 'deposit) 50)
((acc 'some-password 'deposit) 50)
((acc 'some-password 'deposit) 50)