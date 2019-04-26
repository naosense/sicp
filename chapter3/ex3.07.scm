#lang sicp

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
  (define (dispatch pass m)
    (if (eq? password pass)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT"
                           m)))
        (error "Incorrect password"))) 
  dispatch)

(define (make-joint account old-pass new-pass)
  (lambda (pass m) 
    (if (eq? pass new-pass) 
        (account old-pass m) 
        (error "Incorrect new pass"))))

(define peter-acc (make-account 100 'peter-password))
(define paul-acc (make-joint peter-acc 'peter-password 'paul-password))

((peter-acc 'peter-password 'withdraw) 40)
((paul-acc 'paul-password 'withdraw) 40)
((paul-acc 'paul-password 'deposit) 30)
((paul-acc 'paul-password 'withdraw) 40)