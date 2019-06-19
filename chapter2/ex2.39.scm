#lang sicp

(#%require rackunit)

(define (reverse-1 sequence)
  ;; x: car
  ;; y: result
  ;; (append result (list car))
  (fold-right (lambda (x y) (append y (list x))) nil sequence))

(define (reverse-2 sequence)
  ;; x: result
  ;; y: car
  ;; (append (list car) result)
  (fold-left (lambda (x y) (append (list y) x)) nil sequence))

;; 2.38
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (fold-right op initial sequence)
  (accumulate op initial sequence))

;; common
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


(check-equal? (reverse-1 '(1 2 3)) (reverse-2 '(1 2 3)))
