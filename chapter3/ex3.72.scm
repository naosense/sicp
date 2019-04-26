#lang sicp

(#%require "text.scm")

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
          (let ((s1car (stream-car s1))
                (s2car (stream-car s2))
                (w1 (weight s1))
                (w2 (weight s2)))
            (cond ((< w1 w2)
                   (cons-stream s1car
                                (merge-weighted  (stream-cdr s1) s2 weight)))
                  ((> w1 w2)
                   (cons-stream s2car
                                (merge-weighted  s1 (stream-cdr s2) weight)))
                  (else
                    (cons-stream s1car
                                 (cons-stream s2car ;; 注意不要少了s2car
                                              (merge-weighted  (stream-cdr s1)
                                                               (stream-cdr s2) weight)))))))))

(define (weighted-pairs s t weight)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (merge-weighted
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
      weight)))

(define (square-three-weight s)
  (square-three-weight-pair (stream-car s)))

(define (square-three-weight-pair x)
  (let ((i (car x))
        (j (cadr x)))
    (+ (* i i) (* j j))))

(define square-three-pre
  (weighted-pairs integers integers square-three-weight))

(define (square-three s)
  (let ((curr (stream-ref s 0))
        (next (stream-ref s 1))
        (nnxt (stream-ref s 2)))
    (if (and (= (square-three-weight-pair curr)
                (square-three-weight-pair next))
             (= (square-three-weight-pair curr)
                (square-three-weight-pair nnxt)))
        (cons-stream (list (square-three-weight-pair curr) curr next nnxt)
                     (square-three (stream-cdr s)))
        (square-three (stream-cdr s)))))

(define square-three-stream
  (square-three square-three-pre))

(display-stream-n square-three-stream 5)
