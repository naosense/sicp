#lang sicp

(#%require "text.scm")

(define (show x)
  (display-line x)
  x)

;; return 0
(define x (stream-map show (stream-enumerate-interval 0 10)))

;; 0
;; 1
;; 2
;; 3
;; 4
;; 5
;; return 5
(stream-ref x 5)
;; 6
;; 7
;; return 7
(stream-ref x 7)
