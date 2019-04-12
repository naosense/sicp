#lang sicp

(#%provide (all-defined))

(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))

;(withdraw 25)
;(withdraw 25)
;(withdraw 60)

; 将balance限制到局部变量
(define new-withdraw 
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

;(new-withdraw 25)
;(new-withdraw 25)
;(new-withdraw 60)

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

;(define W1 (make-withdraw 100))
;(define W2 (make-withdraw 100))
;(W1 50)
;(W2 70)

(define (make-account balance)
  ; 在这里定义和在外面定义对环境的影响
  ; 如果在外面定义balance就是全部环境中的balance，
  ; 而如果在这里定义balance就是make-account运行环境中的balance
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

;(define acc (make-account 100))
;((acc 'withdraw) 50)
;((acc 'withdraw) 60)
;((acc 'deposit) 40)
;((acc 'withdraw) 60)

(define rand
  (lambda () (random 4294967087)))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

;(estimate-pi 1000000)