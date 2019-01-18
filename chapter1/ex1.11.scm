#lang sicp

(#%require rackunit)

;; f(n)=n if n<3 and f(n)=f(n-1)+2f(n-2)+3f(n-3) if n≥3.
;; 递归版本
(define (f-r n)
  (if (< n 3)
      n
      (+ (f-r (- n 1))
         (* 2 (f-r (- n 2)))
         (* 3 (f-r (- n 3))))))

;; 迭代版本
(define (f-i n)
  (define (f-iter count c b a)
    (cond ((< count 0) count)
          ((= count 0) a)
          ;; c = c + 2b + 3a, b = c, a = b
          (else (f-iter (- count 1) (+ c (* 2 b) (* 3 a)) c b))))
  (f-iter n 2 1 0))

(check-eq? (f-r -1) (f-i -1))
(check-eq? (f-r 0) (f-i 0))
(check-eq? (f-r 1) (f-i 1))
(check-eq? (f-r 2) (f-i 2))
(check-eq? (f-r 5) (f-i 5))
(check-eq? (f-r 17) (f-i 17))