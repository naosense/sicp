#lang sicp

(#%require sicp-pict)

(define (split first then)
  (define split-func
    (lambda (painter n)
      (if (= n 0)
          painter
          (let ((smaller (split-func painter (- n 1))))
            (first painter (then smaller smaller))))))
  split-func)

(define right-split (split beside below))

(define up-split (split below beside))

(paint (right-split einstein 4))

(paint (up-split einstein 4))

