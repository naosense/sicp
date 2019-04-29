#lang racket

(require sicp-pict)

(define border-painter
  (segments->painter
   (list (make-segment (make-vect 0 0.25)
                       (make-vect 1 0.25))
         (make-segment (make-vect 1 0.25)
                       (make-vect 1 0.75))
         (make-segment (make-vect 1 0.75)
                       (make-vect 0 0.75))
         (make-segment (make-vect 0 0.75)
                       (make-vect 0 0.25)))))

(paint border-painter)

(define x-painter
  (segments->painter
   (list (make-segment (make-vect 0 0)
                       (make-vect 1 1))
         (make-segment (make-vect 1 0)
                       (make-vect 0 1)))))

(paint x-painter)

(define diamond-painter
  (segments->painter
   (list (make-segment (make-vect 0 0.5)
                       (make-vect 0.5 0.25))
         (make-segment (make-vect 0.5 0.25)
                       (make-vect 1 0.5))
         (make-segment (make-vect 1 0.5)
                       (make-vect 0.5 0.75))
         (make-segment (make-vect 0.5 0.75)
                       (make-vect 0 0.5)))))

(paint diamond-painter)

;; wave见pict.scm
