#lang sicp

(#%require "text.scm")

;; double
(define s (cons-stream 1 (add-streams s s)))

(display-stream-n s 10)