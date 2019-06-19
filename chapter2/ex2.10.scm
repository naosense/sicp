#lang sicp

(define (make-interval  a b)
  (cons a b))

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

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
;; solution
(define (div-interval x y)
  (if (< (* (upper-bound y) (lower-bound y)) 0)
      (error "interval contains 0")
      (mul-interval x
                    (make-interval (/ 1 (upper-bound y))
                                   (/ 1 (lower-bound y))))))

(define iv1 (make-interval -1 2))
(define iv2 (make-interval 3 7))
(div-interval iv1 iv2)
(div-interval iv2 iv1)
