#lang sicp

(#%require "text.scm")

;; a)
(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define (integrate-series series)
  (stream-map / series integers))

(display-stream-n (integrate-series ones) 10)

;; b)
(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(display-stream-n exp-series 10)

(define cosine-series
  (cons-stream 1 (integrate-series (scale-stream sine-series -1))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

(display-stream-n cosine-series 10)
(display-stream-n sine-series 10)
