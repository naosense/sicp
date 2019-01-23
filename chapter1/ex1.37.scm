#lang sicp

(#%require rackunit)
(#%require "text.scm")

(#%provide (all-defined))
;; 递归版本
(define (cont-frac-rc n d k)
  (define (rec i)
    (if (> i k)
        0
        (/ (n i)
           (+ (d i)
              (rec (+ i 1))))))
  (rec 1))


;; 迭代版本
(define (cont-frac-it n d k)
  (define (iter count result)
    (if (= count 0)
        result
        (iter (- count 1)
              (/ (n count)
                 (+ (d count)
                    result)))))
  (iter k 0))
  
(check-eqv? (cont-frac-rc (lambda (i) 1.0) (lambda (i) 1.0) 100)
            (cont-frac-it (lambda (i) 1.0) (lambda (i) 1.0) 100))


