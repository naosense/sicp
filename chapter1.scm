(define (fib n)
  (cond ((<= n 1) n)
        (else (+ (fib (- n 1)) (fib (- n 2))))))

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

; square
(define (square x) (* x x))

; exercise 1.3
(define (max-two x y z)
  (define (min-three x y z)
    (cond ((>= x y) (if (>= y z) z y))
          (else (if (>= x z) z x))))
  (- (+ x y z) (min-three x y z)))

; 测试程序是正则序，还是应用序,运行(test 0 (p))卡死
(define (p) (p))

(define (test x y)
  (if (= x 0)
    0
    y))

; 牛顿法求平方根
(define (sqrt-iter guess x)
  (if (good-enough2? guess x)
    guess
    (sqrt-iter (improve guess x)
               x)))
(define (improve guess x)
  (average guess (/ x guess)))
(define (average x y)
  (/ (+ x y) 2))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (sqrt x)
  (sqrt-iter 1.0 x))

; 1.6
; cond和if应该都进行了特殊处理，只会执行某一条分支
; 而new-if只是一个普通的函数，在进行运算时，如果是
; 应用序求值，两个分支都会进行变量替换，无形当中进
; 了运算，如果过程中有递归操作，就会陷入死循环。
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))
; 1.7
; 应该是数比较小的时候不能工作，是不是数比较大的时候也不行，会溢出？不过
; 我实验了20位的数值也可以。
; 只需要重新定义一个good-enough函数即可
(define (good-enough2? guess x)
  (< (abs (/ (- guess (improve guess x)) guess)) 0.001))

; 1.8
; 求立方根只需要改写下improve就行
(define (improve-cbr guess x)
  (/ (+ (/ x (square y)) (* 2 y)) 3)

