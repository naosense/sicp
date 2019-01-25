#lang sicp

(#%require "text.scm")

;; 区间减法应该如何定义？
(define (sub-interval x y)
  (let ((p1 (- (lower-bound y)))
        (p2 (- (upper-bound y))))
    ;; min max可以去掉- (lower-bound y)恒大于- (upper-bound y)
    (add-interval x
                  (make-interval p2 p1))))



(define iv1 (make-interval -1 2))
(define iv2 (make-interval 3 7))

(sub-interval iv2 iv1)
(sub-interval iv1 iv2)
        