#lang sicp

(#%require "text.scm")

(define (stream-map proc . argsstreams)
  (if (stream-null? (car argsstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argsstreams))
       (apply stream-map
              (cons proc (map stream-cdr argsstreams))))))

(display-stream (stream-map +
                            (stream-enumerate-interval 1 3)
                            (stream-enumerate-interval 4 6)
                            (stream-enumerate-interval 7 9)))

