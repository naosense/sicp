#lang sicp

(#%require "text.scm")

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))

(define memo-fib
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))


(memo-fib 100)

;; memo-fib在memoize的运行环境E1中创建，且在E1中不停地调用memo-fib
;; 所以可以不停地查表避免重复计算，而直接(memoize fib)不能达到这样的
;; 效果，因为fib中没有查表
