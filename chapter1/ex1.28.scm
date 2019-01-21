#lang sicp

(#%require rackunit)
(#%require "text.scm")

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (if (unnormal-sqrt? exp)
             0
             (remainder (square (expmod base (/ exp 2) m))
                    m)))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                     m))))

;; 是否有不等于1和n-1的非平凡平方根
(define (unnormal-sqrt? n)
  (define (iter a)
    (cond ((= a 1) false)
          ((= (remainder (square a) n) 1) true)
          (else (iter (- a 1)))))
  (iter (- n 1)))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (prob-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (fast-prime? n (- times 1)))
        (else false)))


(check-false (prob-prime? 561 561))
(check-true (fast-prime? 561 561))
(check-false (prob-prime? 1105 1105))
(check-true (fast-prime? 1105 1105))
(check-false (prob-prime? 1729 1729))
(check-true (fast-prime? 1729 1729))
(check-false (prob-prime? 2465 2465))
(check-true (fast-prime? 2465 2465))
(check-false (prob-prime? 2821 2821))
(check-true (fast-prime? 2821 2821))
(check-false (prob-prime? 6601 6601))
(check-true (fast-prime? 6601 6601))