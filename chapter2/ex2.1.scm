#lang sicp

(#%require "text.scm")

(define (make-rat-a n d)
  (let ((g (gcd n d)))
    (if (< (* n d) 0)
        (cons (- (abs (/ n g))) (abs (/ d g)))
        (cons (abs (/ n g)) (abs (/ d g))))))
  

(print-rat (make-rat-a 1 -3))
(print-rat (make-rat-a -2 6))
(print-rat (make-rat-a -2 -6))
(print-rat (make-rat-a 2 6))



        
