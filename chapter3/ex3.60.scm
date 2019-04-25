#lang sicp

(#%require "text.scm")

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define (integrate-series series)
  (stream-map / series integers))

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1 (integrate-series (scale-stream sine-series -1))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

;; 一开始理解错了，以为s1是系数，s2是x，实际上mul-series是两个系数之间的乘积
;; 比如s1: 1 2 3... s2: 1 2 4...，那么结果应该是
;; 1 2 4  8  16
;;   2 4  8  16
;;     3  6  12
;;        4  8  16
;;+
;;=1 4 11 26 ...
;; 可以看到如果是s1如果是ones的话相当于对s2各项求和
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (scale-stream (stream-cdr s2) (stream-car s1)) 
                            (mul-series (stream-cdr s1) s2))))


(define (pow-stream x)
  (cons-stream 1 (cons-stream x (scale-stream (stream-cdr (pow-stream x)) x))))

(display-stream-n (mul-series integers (pow-stream 2)) 10)

;; e^1
(display-stream-n (mul-series (mul-streams exp-series (pow-stream 1.0)) ones) 10)

(define sine-pi (mul-streams sine-series (pow-stream pi)))

(define cosine-pi (mul-streams cosine-series (pow-stream pi)))

;; sine(pi)=0
(display-stream-n (mul-series sine-pi ones) 10)
;; cosine(pi)=-1
(display-stream-n (mul-series cosine-pi ones) 10)

;; sine(pi)^2 + cosine(pi)^2 = 1
(display-stream-n (add-streams (mul-streams (mul-series sine-pi ones)
                                            (mul-series sine-pi ones))
                               (mul-streams (mul-series cosine-pi ones)
                                            (mul-series cosine-pi ones)))
                  10)