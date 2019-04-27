#lang sicp

(#%require "text.scm")

(define (RLC R L C dt)
  (lambda (vc0 il0)
    (define vc (integral (delay (force dvc)) vc0 dt))
    (define il (integral (delay (force dil)) il0 dt))
    (define dvc (delay (scale-stream il (/ -1 C))))
    (define dil (delay (add-streams (scale-stream vc (/ 1 L))
                             (scale-stream il (/ (- R) L)))))
    (stream-map cons vc il)))

(display-stream-n ((RLC 1 0.2 1 0.1) 10 0) 10)
