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

; 函数运行环境的外围环境(父环境)是创建函数的环境,测试程序:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xx 10)

(define (global-f)
  xx)

(define (env-test)
  (define (local-f) xx)
  (define xx 1)
  (cons (global-f) (local-f)))

;(global-f)
;(env-test)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define x (list 'a 'b))
(define z1 (cons x x))
(define z2 (cons (list 'a 'b) (list 'a 'b)))
; 队列
(define (front-ptr queue) (car queue))

(define (rear-ptr queue) (cdr queue))

(define (set-front-ptr! queue item)
  (set-car! queue item))

(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))

;; 一维表格
(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) (cdr table)))))
  'ok)

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (make-table)
  (list '*table*))

(define t1 (make-table))
;(insert! 'a 1 t1)
;t1
;(insert! 'b 2 t1)
;t1

; 二维表格
;(define (lookup key-1 key-2 table)
;  (let ((subtable (assoc key-1 (cdr table))))
;    (if subtable
;        (let ((record (assoc key-2 (cdr subtable))))
;          (if record
;              (cdr record)
;              false))
;        false)))
;
;(define (insert! key-1 key-2 value table)
;  (let ((subtable (assoc key-1 (cdr table))))
;    (if subtable
;        (let ((record (assoc key-2 (cdr subtable))))
;          (if record
;             (set-cdr! record value)
;             (set-cdr! subtable
;                  (cons (cons key-2 value) (cdr subtable)))))
;        (set-cdr! table
;                  (cons ;;(cons key-1 (list (cons key-2 value)))亦可
;                        (list key-1 (cons key-2 value))
;                        (cdr table)))))
;  'ok)
;
;(insert! 'math '+ 43 t1)
;(insert! 'math '- 45 t1)
;(insert! 'letters 'a 97 t1)
;(insert! 'letters 'b 98 t1)
;(lookup 'math '- t1)
;(lookup 'letters 'b t1)