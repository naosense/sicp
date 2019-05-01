#lang sicp

(#%require "text.scm")

(define (and? exp) (tagged-list? exp 'and))

(define (and-actions exp) (cdr exp))

(define (no-action? actions) (null? actions))

(define (last-action? actions) (null? (cdr actions)))

(define (first-action actions) (car actions))

(define (rest-actions actions) (cdr actions))

(define (eval-and actions env)
  (cond ((no-action? actions) 'true)
        ((last-action? actions)
         (eval (first-action actions) env))
        ((eval (first-action actions) env)
         (eval-and (rest-actions actions) env))
        (else 'false)))

;; or和and非常类似，只有or?和eval-or不一样
(define (or? exp) (tagged-list? exp 'or))

(define (eval-or actions env)
  (cond ((no-action? actions) 'false)
        ((last-action? actions)
         (eval (first-action actions) env))
        ((eval (first-action actions) env) 'true)
        (else
          (eval-or (rest-actions actions) env))))

;; 派生表达式
;; (and exp1 exp2 ...)等价于
;; (if (not (exp1))
;;     false
;;     (if (not (exp2))
;;         false
;;         ...
;; (or exp1 exp2 ...)等价于
;; (if (exp1)
;;     true
;;     (if (exp2)
;;         true
;;         ...
