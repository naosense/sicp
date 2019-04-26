#lang sicp

(#%require "text.scm")

(define (triples s t u)
  (cons-stream
    (list (stream-car s) (stream-car t) (stream-car u))
    (interleave
      (stream-map (lambda (x) (cons (stream-car s) x)) ;; 注意这里是cons
                  (stream-cdr (pairs t u)))
      (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define phythagorean-stream
  (stream-filter
    (lambda (x)
      (let ((i (car x))
            (j (cadr x))
            (k (caddr x)))
        (= (+ (square i) (square j)) (square k))))
    (triples integers integers integers)))


(display-stream-n (triples integers integers integers) 10)
(display-stream-n phythagorean-stream 10)
