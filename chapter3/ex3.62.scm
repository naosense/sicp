#lang sicp

(#%require "text.scm")

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define (pow-stream x)
  (cons-stream 1 (cons-stream x (scale-stream (stream-cdr (pow-stream x)) x))))

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
                            (mul-series (stream-cdr s1) s2))))

(define (multiplicative-inverse s)
  (cons-stream 1 (scale-stream
                  (mul-series (stream-cdr s)
                              (multiplicative-inverse s))
                  -1)))

(define (div-series s1 s2)
  (let ((dividend (stream-car s2)))
    (if (= dividend 0)
        (error "divide zero /0")
        (mul-series s1
                    (multiplicative-inverse s2)))))

(define (integrate-series series)
  (stream-map / series integers))

(define cosine-series
  (cons-stream 1 (integrate-series (scale-stream sine-series -1))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

(define sine-pi/4 (mul-streams sine-series (pow-stream (/ pi 4))))

(define cosine-pi/4 (mul-streams cosine-series (pow-stream (/ pi 4))))

;; tan(pi/4)=1
(define tan-pi/4 (div-series sine-pi/4 cosine-pi/4))
;; 可以看到逐渐接近1
(display-stream-n (mul-series tan-pi/4 ones) 10)
