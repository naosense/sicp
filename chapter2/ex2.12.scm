#lang sicp

;; solution
(define (make-center-percent c p)
  (let ((r (/ p 100.0)))
    (make-interval (* c (- 1 r))
                   (* c (+ 1 r)))))

(define (percent i)
  (* (/ (- (upper-bound i) (lower-bound i))
        (center i))
     50))

;; common
(define (make-interval  a b)
  (cons a b))

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))


(percent (make-center-percent 35 21))
