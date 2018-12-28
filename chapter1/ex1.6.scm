#lang sicp

;; 1.6
;; cond和if应该都进行了特殊处理,只会执行某一条分支
;; 而new-if只是一个普通的函数,在进行运算时,如果是
;; 应用序求值,两个分支都会进行变量替换,无形当中进
;; 了运算,如果过程中有递归操作,就会陷入死循环。
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))