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

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1 (upper-bound y))
                               (/ 1 (lower-bound y)))))

;; 区间减法应该如何定义？
(define (sub-interval x y)
  (let ((p1 (- (lower-bound y)))
        (p2 (- (upper-bound y))))
    ;; min max可以去掉- (lower-bound y)恒大于- (upper-bound y)
    (add-interval x
                  (make-interval p2 p1))))


;; solution
(define iv1 (make-interval -1 2))
(define iv2 (make-interval 3 7))

(sub-interval iv2 iv1)
(sub-interval iv1 iv2)
