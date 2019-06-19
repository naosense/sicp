#lang sicp

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

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ".")
  (display (y-point p))
  (display ")"))

(define (mid-point segment)
  (let ((start (start-segment segment))
        (end (end-segment segment)))
    (make-point (average (x-point start)
                         (x-point end))
                (average (y-point start)
                         (y-point end)))))

(define (average x y)
  (/ (+ x y) 2))

(print-point (mid-point (make-segment (make-point 1 2) (make-point 5 8)))) 
