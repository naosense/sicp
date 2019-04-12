#lang sicp

(define f
  (let ((cache -1))
    (lambda (a)
      (if (= cache 1)
          1
          (begin (set! cache a) 0)))))
          
(+ (f 0) (f 1))
  