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

; 1.5
; 测试程序是正则序，还是应用序,运行(test 0 (p))卡死
; 如果是应用序，p将会不停的递归运算，将会陷入死循环，
; 如果是正则序，将会得到0
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
; 求立方根只需要改写下improve，good-enough就行
(define (improve-cbr guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3.0))
(define (good-enough-cbr? guess x) 
  (< (abs (- (cube guess) x) 0.001)))
(define (cube x)
  (* x x x))

; 递归
(define (factorial n)
  (if (= n 1)
    1
    (* n (factorial (- n 1)))))

; 迭代
(define (factorial n)
  (fact-iter 1 1 n))
(define (fact-iter product counter max-count)
  (if (> counter max-count)
    product
    (fact-iter (* counter product)
               (+ counter 1)
               max-count)))

; 1.9
; 前者是递归计算，后者是迭代计算
; (define (+ a b)
;   (if (= a 0)
;     b
;     (inc (+ (dec a) b))))
; (define (+ a b)
;   (if (= a 0)
;     b
;     (+ (dec a) (inc b))))

; 1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))
; 2*n
(define (f n) (A 0 n))
; 2^n
(define (g n) (A 1 n))
; 2^(2^n-1)
(define (h n) (A 2 n))

(define (fib n)
  (cond ((<= n 1) n)
        (else (+ (fib (- n 1)) (fib (- n 2))))))

(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))

; 1.11
(define (f n)
  (if (< n 3)
    n
    (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

(define (f n)
  (define (f-iter a b c count)
    (cond ((= count 0) c)
          ((< count 0) count)
          (else (f-iter (+ a (* 2 b) (* 3 c)) a b (- count 1)))))
  (f-iter 2 1 0 n))

; 1.12
(define (pascal r c) 
   (if (or (= c 1) (= c r)) 
       1 
       (+ (pascal (- r 1) (- c 1)) (pascal (- r 1) c)))) 

(define (expt b n)
  (if (= n 0)
    1
    (* b (expt  b (- n 1)))))

(define (expt b n)
  (define (expt-iter b counter product)
    (if (= counter 0)
       product
       (expt-iter b (- counter 1) (* b product))))
  (expt-iter b n 1))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (even? n)
  (= (remainder n 2) 0))

; 1.16
; 迭代方式的快速求幂
(define (fast-expt b n)
  (define (fast-expt-iter b a counter)
    (cond ((= counter 0) a)
          ((= counter 1) (* a b))
          ((even? counter) (fast-expt-iter 
                             (square b)
                             (* (square b) a)
                             (- (/ counter 2) 1)))
          (else (fast-expt-iter 
                  (square b) 
                  (* (cube b) a)
                  (- (/ (- counter 1) 2) 1)))))
  (fast-expt-iter b 1 n))

; 1.17
(define (mul a b)
  (cond ((= b 0) 0)
        ((even? b) (double (mul a (halve b))))
        (else (+ a (double (mul a (halve (- b 1))))))))
(define (double x)
  (+ x x))
(define (halve x)
  (/ x 2))

; 1.18
(define (mul a b)
  (define (mul-iter r a counter)
    (cond ((= counter 0) r)
          ((even? counter) (mul-iter r (double a) (halve counter)))
          (else (mul-iter (+ r a) a (- counter 1)))))
  (mul-iter 0 a b))
