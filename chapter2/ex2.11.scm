#lang sicp

(#%require "text.scm")

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

(define iv1 (make-interval -1 2))
(define iv2 (make-interval 3 7))

(mul-interval iv1 iv2)
(mul-interval-a iv1 iv2)
        
    