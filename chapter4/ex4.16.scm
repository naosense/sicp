#lang sicp

(#%require "lazy.scm")

;; a)已经在text.scm中修改

;; b)


;; eg
(define lbd '(lambda (x y)
               (define u 1)
               (define v 2)
               (define (w)
                 (define a 1)
                 (+ a 1))
               (+ u v w)))
;;(display (inner-vars (lambda-body lbd)))
;;(display (inner-vals (lambda-body lbd)))
;;(display (inner-exps (lambda-body lbd)))
;;(display (map (lambda (var) (list var '*unassigned*)) (inner-vars (lambda-body lbd))))
;;(display (map (lambda (var val) (list 'set var val))
;;              (inner-vars (lambda-body lbd))
;;              (inner-vals (lambda-body lbd))))

;; from ex4.6
(define (make-let locals body)
  (cons 'let (cons locals body)))

(define (scan-out-defines lbody)
  (define (inner-vars body)
    (cond ((null? body) '())
          ((definition? (car body))
           (cons (definition-variable (car body))
                 (inner-vars (cdr body))))
          (else (inner-vars (cdr body)))))
  (define (inner-vals body)
    (cond ((null? body) '())
          ((definition? (car body))
           (cons (definition-value (car body))
                 (inner-vals (cdr body))))
          (else (inner-vals (cdr body)))))
  (define (inner-exps body)
    (cond ((null? body) '())
          ((definition? (car body))
           (inner-exps (cdr body)))
          (else body)))

  (let ((parameters (lambda-parameters lbody))
        (body (lambda-body lbody)))
    (let ((vars (inner-vars body))
          (vals (inner-vals body))
          (exps (inner-exps body)))
      (make-lambda parameters
                   (list (make-let (map (lambda (var) (list var '*unassigned*)) vars)
                                   (append (map (lambda (var val) (list 'set! var val)) vars vals)
                                           exps)))))))

(display (scan-out-defines lbd))

;; c) 放在make-procedure比较合适，首先它不是一个基本形式，没有特殊的tag标识
;; 放在eval不合适，其他如果放在procedure-body中，这样调用一次就会扫描一次，浪费资源
