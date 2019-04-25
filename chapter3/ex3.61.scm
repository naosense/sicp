#lang sicp

(#%require "text.scm")

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (scale-stream (stream-cdr s2) (stream-car s1)) 
                            (mul-series (stream-cdr s1) s2))))

(define (integrate-series series)
  (stream-map / series integers))

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define (multiplicative-inverse s)
  (cons-stream 1 (scale-stream
                  (mul-series (stream-cdr s)
                              (multiplicative-inverse s))
                  -1)))

(display-stream-n (multiplicative-inverse exp-series) 10)