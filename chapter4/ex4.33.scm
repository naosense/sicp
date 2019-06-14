#lang sicp

;; text-of-quotation作如下修改
(define (text-of-quotation exp env)
  (let ((promise (cadr exp)))
    (if (pair? promise)
        (eval (literal->list promise) env)
        promise)))

;; 注意里面的几个quote使用
(define (literal->list exp)
  (if (null? exp)
      (list 'quote '())
      (list 'cons
            (list 'quote (car exp))
            (literal->list (cdr exp)))))

