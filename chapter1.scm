(declare (usual-integrations))

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
  (if (good-enough? guess x)
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

