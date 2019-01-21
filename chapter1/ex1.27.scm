#lang sicp

(#%require rackunit)
(#%require "text.scm")

(define (fermat-test n a)
  ;; a^n % n == a
  (= (expmod a n n) a))

(define (carmichael-check n)
  (define (iter a)
    (if (= a 0)
        ((lambda () (display n)(display " is carmichael")(newline) true))
        (if (fermat-test n a)
          (iter (- a 1))
          ((lambda () (display n)(display " is not prime")(newline) false)))))
  (iter (- n 1)))


(check-true (carmichael-check 561))
(check-true (carmichael-check 1105))
(check-true (carmichael-check 1729))
(check-true (carmichael-check 2465))
(check-true (carmichael-check 2821))
(check-true (carmichael-check 6601))