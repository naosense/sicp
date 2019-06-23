#lang sicp

(define (swap idx1 idx2 lst)
  (define (build-list lst idx e1 e2)
    (cond ((null? lst) '())
          ((= idx idx1)
           (cons e2 (build-list (cdr lst) (+ idx 1) e1 e2)))
          ((= idx idx2)
           (cons e1 (build-list (cdr lst) (+ idx 1) e1 e2)))
          (else
            (cons (car lst) (build-list (cdr lst) (+ idx 1) e1 e2)))))
  (build-list lst 0 (list-ref lst idx1) (list-ref lst idx2)))

(define (shuffle lst)
  (define (iter i lst)
    (if (= i (length lst))
        lst
        (iter (+ i 1) (swap i (random (length lst)) lst))))
  (iter 0 lst))

(define (analyze-amb exp)
  (let ((cprocs (shuffle (map analyze (amb-choices exp))))) ;; changed
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices) env
                           succeed
                           (lambda ()
                             (try-next (cdr choices))))))
      (try-next cprocs))))
