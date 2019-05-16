#lang sicp

(#%require "text.scm")

;; a)
(define letrec-eg '(letrec ((var1 exp1) (var2 exp2))
                     exp3))

(define (letrec? exp) (tagged-list? exp 'letrec))

(define (letrec-variables exp)
  (map car (cadr exp)))

(define (letrec-values exp)
  (map cadr (cadr exp)))

(define (letrec-body exp)
  (cddr exp))

;; (display (letrec-variables letrec-eg))
;; (display (letrec-values letrec-eg))
;; (display (letrec-body letrec-eg))

(define (make-let locals body)
  (cons 'let (cons locals body)))

(define (letrec->let exp)
  (let ((vars (letrec-variables exp))
        (vals (letrec-values exp))
        (body (letrec-body exp)))
  (make-let (map (lambda (var) (list var ''*unassigned*)) vars)
            (append (map (lambda (var val) (list 'set! var val)) vars vals)
                    body))))

(display (letrec->let letrec-eg))

;; b)
;; (letrec ((fact
;;           (lambda (n)
;;             (if (= n 1)
;;                 1
;;                 (* n (fact (- n 1)))))))
;;   (fact 10))
