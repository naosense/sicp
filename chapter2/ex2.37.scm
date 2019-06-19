#lang sicp

(#%require rackunit)

(define (dot-product v w)
  (define (map proc x y)
  (if (null? x)
      nil
      (cons (proc (car x) (car y))
            (map proc (cdr x) (cdr y)))))
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (w) (dot-product v w)) m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (v) (matrix-*-vector cols v)) m)))

;; 2.37
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      ;; 齐头并进
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;; common
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define m '((1 2 3 4) (4 5 6 6) (6 7 8 9)))
(define n '((1 2) (4 5) (6 7) (8 9)))

(display (dot-product '(1 2 3) '(4 5 6)))
(newline)
(display (matrix-*-vector m '(1 2 3 4)))
(newline)
(display (transpose m))
(newline)
(display (matrix-*-matrix m n))
