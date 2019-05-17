#lang sicp

(#%require "text.scm")

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))
      false))


(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes n count)
  (if (= count 0)
      (display "\nend\n")
      (if (timed-prime-test n)
          (search-for-primes (+ n 2) (- count 1))
          (search-for-primes (+ n 2) count))))

;; 2000-3000
(search-for-primes 1000000001 3)
;; 6000-7000
(search-for-primes 10000000001 3)
;; 21000-25000
(search-for-primes 100000000001 3)
