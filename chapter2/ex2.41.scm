#lang sicp

(#%require rackunit)

(define (order-three-sum n s)
  (define (three-sum? pair)
    (= (+ (car pair) (cadr pair) (caddr pair)) s))
  (filter three-sum?
          (flatmap
           (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k)
                               (list i j k))
                             (enumerate-interval (+ j 1) n)))
                      (enumerate-interval (+ i 1) n)))
           (enumerate-interval 1 n))))

;; common
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))



(display (order-three-sum 10 6))
