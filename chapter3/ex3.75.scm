#lang sicp

(#%require "text.scm")

(define (stream-of-list lst)
  (if (null? lst)
      the-empty-stream
      (cons-stream (car lst) (stream-of-list (cdr lst)))))

(define input '(1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4))

(define sense-data (stream-of-list input))

;; (display-stream sense-data)

(define (sign-change-detector input last-value)
  (cond ((and (< input 0)
              (>= last-value 0))
         1)
        ((and (>= input 0)
              (< last-value 0))
         -1)
        (else 0)))

(define (make-zero-crossing input-stream last-value last-avpt)
  (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
    (cons-stream (sign-change-detector avpt last-avpt)
                 (make-zero-crossing (stream-cdr input-stream)
                                     (stream-car input-stream)
                                     avpt))))

(define zero-crossing (make-zero-crossing sense-data 0 0))

(display-stream-n  zero-crossing (- (length input) 1))
