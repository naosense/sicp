#lang sicp

(#%require "lazy.scm")

;; 执行如下程序，
;; 分析和执行在一起的结果   257000
;; 分析和执行分开的结果     139000
;; 从结果上前者大约是后者的2倍，
;; 说明重复分析的时间大于与执行时间相当
;; 当然不同的程序可能也不一样
((lambda ()
  (define (fib n)
    (cond ((= n 0) 0)
          ((= n 1) 1)
          (else (+ (fib (- n 1))
                   (fib (- n 2))))))
  (define start (runtime))
  (display (fib 20))
  (- (runtime) start)))
