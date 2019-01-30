#lang sicp

(#%require rackunit)
(#%require "text.scm")

(define (order-three-sum n s)
  (define (three-sum? pair)
    (= (+ (car pair) (cadr pair) (caddr pair)) s))
  (filter three-sum?
          (flatmap
           (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k)
                               (list i j k))
                             (enumerate-interval (+ j 1) n)))
                      (enumerate-interval (+ i 1) n)))
           (enumerate-interval 1 n))))

  

(display (order-three-sum 10 6))



