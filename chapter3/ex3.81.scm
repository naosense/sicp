#lang sicp

(#%require "text.scm")

(define (random-int initial)
  (cons-stream initial
               (random-int (rand-update initial))))

;; (display-stream-n (random-int 100) 10)
(define initial 100)
(define (rand-with-reset m)
  (define (generate-stream)
    (random-int initial))
  (define (reset-stream new-value)
    (random-int new-value))
  (cond ((eq? m 'generate) generate-stream)
        ((eq? m 'reset) reset-stream)
        (else (error "Unknown request -- rand-with-reset"))))

(display-stream-n ((rand-with-reset 'generate)) 10)
(display-stream-n ((rand-with-reset 'reset) 1) 10)
