#lang sicp

(#%require rackunit)
(#%require "text.scm")

(define (iterative-improve good-enough? improve)
  (lambda (guess)    
    (if (good-enough? guess)
        guess
        ((iterative-improve good-enough? improve)
         (improve guess)))))

(define (sqrt-it x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improve) 1.0))

(check-eqv? (sqrt-it 2) (sqrt 2))
(check-eqv? (sqrt-it 3) (sqrt 3))
(check-eqv? (sqrt-it 5) (sqrt 5))

(define (fixed-point-it f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  ((iterative-improve (lambda (x) (close-enough? x (f x)))
                      f)
   first-guess))

(check-eqv? (fixed-point cos 1.0) (fixed-point-it cos 1.0))
(check-eqv? (fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)
            (fixed-point-it (lambda (y) (+ (sin y) (cos y))) 1.0))



