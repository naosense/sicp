#lang sicp

;; 整体上只需要修改improve和good-enough?函数就可以了，
;; 基本的脉络和平方根没什么区别
(define (cube x)
  (* x x x))

(define (cbrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (cbrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (/ (+ (/ x (* guess guess))
        (* 2 guess))
     3))

(define (good-enough? guess x)
  (< (abs (- (cube guess) x)) 0.001))

(define (cbrt x)
  (cbrt-iter 1.0 x))

(cbrt 27)
(cbrt 0.001)
(cbrt 0.000001)