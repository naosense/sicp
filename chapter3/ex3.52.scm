#lang sicp

(#%require "text.scm")

(define sum 0)
(display-line (string-append "sum = " (number->string sum)))
(define (accum x)
  (set! sum (+ x sum))
  sum)
(display-line (string-append "sum = " (number->string sum)))

;; seq如果没有延迟计算，应该1 3 6 10 15 21 28...
;; 由于延迟计算，stream-map只计算第一个，即(accum 1) sum=1
(define seq (stream-map accum (stream-enumerate-interval 1 20)))
(display-line (string-append "sum = " (number->string sum)))

;; 跳过3
;; 找到第一个偶数即6, y: 6 10 28 36 66 78 120 136 190 210
(define y (stream-filter even? seq))
(display-line (string-append "sum = " (number->string sum)))


;; 跳过1 3 6
;; 找到第一个整除5的，即10，z：10 15 45 55 105 120 190 210
(define z (stream-filter (lambda (x) (= (remainder x 5) 0)) seq))
(display-line (string-append "sum = " (number->string sum)))
(newline)

;; 计算到y的索引7，即第8个：136
(stream-ref y 7)
(display-line (string-append "sum = " (number->string sum)))
;; 遍历整个z，所以sum最后等于210
(display-stream z)
(display-line (string-append "sum = " (number->string sum)))

;; 如果没有memo-proc将会导致(accum x)多次计算
;; 比如(define y (stream-filter even? seq))将会得到如下序列：
;; enumerate-interval + sum  = seq
;; 1                    1      1   
;; 2                    1->3   3
;; 3                    3->6   6
;; 然后再计算(define z (stream-filter (lambda (x) (= (remainder x 5) 0)) seq))，
;; 如果有memo-proc将会得到如下序列：
;; enumerate-interval + sum  = seq
;; 1                    6      1   （已经计算过直接返回）
;; 2                    6      3   （已经计算过直接返回）
;; 3                    6      6   （已经计算过直接返回）
;; 4                    6->10  10
;; 而如果没有memo-proc将会得到如下序列：
;; enumerate-interval + sum  = seq
;; 1                    6      1   
;; 2                    6->8   8
;; 3                    8->11  11
;; 4                    11->15 15