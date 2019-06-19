#lang sicp

(#%require rackunit)

;; 第一种表示形式为中心点和四个角的任意一点
(define (make-rect1 diagonal-line)
  diagonal-line)

;; 第二种表示形式为任意对角的两个点
(define (make-rect2 corner-p1 corner-p2)
  (make-segment corner-p1 corner-p2))

(define (circumference width height)
  (* 2 (+ width height)))

(define (area width height)
  (* width height))

(define (width rect)
  (* 2 (abs (- (x-point (start-segment rect))
               (x-point (end-segment rect))))))

(define (height rect)
  (* 2 (abs (- (y-point (start-segment rect))
               (y-point (end-segment rect))))))

;; function from 2.02
(define (make-segment p q)
  (cons p q))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define r1 (make-rect1 (make-segment (make-point 0 0) (make-point 1 2))))
(define r2 (make-rect2 (make-point 0 0) (make-point 1 2)))
(check-eqv? (circumference (width r1) (height r1)) (circumference (width r2) (height r2)))
(check-eqv? (area (width r1) (height r1)) (area (width r2) (height r2)))
