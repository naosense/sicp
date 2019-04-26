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

;; a)
(define i+j-stream
  (weighted-pairs integers integers (lambda (s)
                                      (let ((x (stream-car s)))
                                        (+ (car x) (cadr x))))))
(display-stream-n i+j-stream 100)

;; b)
(define 2i+3j+5ij-stream
  (stream-filter
    (lambda (x)
      (let ((i (car x))
            (j (cadr x)))
        (or (divisible? i 2)
            (divisible? i 3)
            (divisible? i 5)
            (divisible? j 2)
            (divisible? j 3)
            (divisible? i 5))))
    (weighted-pairs integers integers (lambda (s)
                                        (let ((x (stream-car s)))
                                          (let ((i (car x))
                                                (j (cadr x)))
                                            (+ (* 2 i) (* 3 j) (* 5 i j))))))))

(display-stream-n 2i+3j+5ij-stream 100)

;; ex3.71
(define (ramanujan-weight s)
  (ramanujan-weight-pair (stream-car s)))

(define (ramanujan-weight-pair x)
  (let ((i (car x))
        (j (cadr x)))
    (+ (* i i i) (* j j j))))

(define ramanujan-pre
  (weighted-pairs integers integers ramanujan-weight))

(display-stream-n ramanujan-pre 100)

(define (ramanujan s)
  (let ((curr (stream-ref s 0))
        (next (stream-ref s 1)))
    (if (= (ramanujan-weight-pair curr)
           (ramanujan-weight-pair next))
        (cons-stream (ramanujan-weight-pair curr)
                     (ramanujan (stream-cdr s)))
        (ramanujan (stream-cdr s)))))

(define ramanujan-stream
  (ramanujan ramanujan-pre))

(display-stream-n ramanujan-stream 5)
