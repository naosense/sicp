#lang sicp

(#%require "text.scm")

(define a1 (make-wire))
(define a2 (make-wire))
(define output (make-wire))
(probe 'output output)

(set-signal! a1 0)
(set-signal! a2 1)
(and-gate a1 a2 output)
;; 这个propagate不能少
(propagate)
(set-signal! a1 1)
(set-signal! a2 0)
(propagate)

;; 如果改成LIFO，
;; 初始情况下a1=0, a2=1，时刻3 out=0
;; 这个时候a1: 0->1, agenda: 1:(lambda () (set-signal! output a1 & a2 = 1 & 1 = 1))
;; 然后a2: 1->0, agenda: 1:(lambda () (set-signal! output a1 & a2 = 1 & 1 = 1))
;; 2:(lambda () (set-signal! output a1 & a2 = 1 & 0 = 0))
;; 如果队列是LIFO，就会先执行2:(lambda () (set-signal! output a1 & a2 = 1 & 0 = 0))，output->0
;; 然后执行1->0, agenda: 1:(lambda () (set-signal! output a1 & a2 = 1 & 1 = 1))，output->1
;; 即最终输出值output=1与事实不符