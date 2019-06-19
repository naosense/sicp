#lang sicp

;; solution
(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))
;; 2.12
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

(define (add-interval x y)
  (make-interval (+ (lower-bound x)
                    (lower-bound y))
                 (+ (upper-bound x)
                    (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1 (upper-bound y))
                               (/ 1 (lower-bound y)))))

(define A (make-center-percent 35 1))
(define B (make-center-percent 100 2))

(div-interval A A)
(div-interval A B)
