#lang sicp

(#%require "text.scm")

(define initial 100)

(define rand-with-reset
  (let ((x initial))
    (define (generate)
      (set! x (rand-update x))
      x)
    (define (reset new-value)
      (set! x new-value)
      x)
    (define (dispatch m)
      (cond ((eq? m 'generate) generate)
            ((eq? m 'reset) reset)
            (else (error "Unknown request -- rand-with-reset"))))
    dispatch))

((rand-with-reset 'generate))
((rand-with-reset 'generate))
((rand-with-reset 'generate))
((rand-with-reset 'generate))
((rand-with-reset 'generate))
((rand-with-reset 'reset) 1)
((rand-with-reset 'generate))
((rand-with-reset 'generate))
((rand-with-reset 'generate))
((rand-with-reset 'generate))
((rand-with-reset 'generate))
