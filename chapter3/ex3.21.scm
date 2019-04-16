#lang sicp

(#%require "text.scm")

(define q1 (make-queue))

(insert-queue! q1 'a)

(insert-queue! q1 'b)

(delete-queue! q1)

(delete-queue! q1)

(define (print-queue queue)
  (define (iter first)
    (cond ((null? first)
           (newline))
          (else
           (display (car first))
           (display " ")
           (iter (cdr first)))))
  (iter (front-ptr queue)))

(print-queue (insert-queue! q1 'a))
(print-queue (insert-queue! q1 'b))
(print-queue (delete-queue! q1))
(print-queue (delete-queue! q1))