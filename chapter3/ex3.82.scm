#lang sicp

(#%require "text.scm")

;; 注意random给整数返回<整数的正整数，给浮点数返回浮点数
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define random-numbers-in-range
  (stream-map random-in-range
              (scale-stream ones -1.0)
              ones))

;; (display-stream-n random-numbers-in-range 10)

(define integral-stream
  (map-successive-pairs (lambda (x y) (<= (+ (square x) (square y)) 1))
                        random-numbers-in-range))

(define integral-val
  (stream-map (lambda (p) (* 4.0 p))
              (monte-carlo-test integral-stream 0 0)))

(stream-ref integral-val 100000)
