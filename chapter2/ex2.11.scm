#lang sicp

;; solution
(define (mul-interval-a x y)
  (let ((a1 (lower-bound x))
        (b1 (upper-bound x))
        (a2 (lower-bound y))
        (b2 (upper-bound y)))
    (cond ((and (< a1 0) (< b1 0))
           (cond ((and (< a2 0) (< b2 0))
                  (make-interval (* b1 b2) (* a1 a2)))
                 ((and (< a2 0) (>= b2 0))
                  (make-interval (* a1 b2) (* a1 a2)))
                 ((and (>= a2 0) (>= b2 0))
                  (make-interval (* a1 b2) (* b1 a2)))))

          ((and (< a1 0) (>= b1 0))
           (cond ((and (< a2 0) (< b2 0))
                  (make-interval (* b1 a2) (* a1 a2)))
                 ;;--这种情况比较特殊
                 ((and (< a2 0) (>= b2 0))
                  (make-interval (min (* a1 b2) (* b1 a2))
                                 (max (* a1 a2) (* b1 b2))))
                 ((and (>= a2 0) (>= b2 0))
                  (make-interval (* a1 b2) (* b1 b2)))))

          ((and (>= a1 0) (>= b1 0))
           (cond ((and (< a2 0) (< b2 0))
                  (make-interval (* b1 a2) (* a1 b2)))
                 ((and (< a2 0) (>= b2 0))
                  (make-interval (* b1 a2) (* b1 b2)))
                 ((and (>= a2 0) (>= b2 0))
                  (make-interval (* a1 a2) (* b1 b2))))))))

;; common functions
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

(define iv1 (make-interval -1 2))
(define iv2 (make-interval 3 7))

(mul-interval iv1 iv2)
(mul-interval-a iv1 iv2)
