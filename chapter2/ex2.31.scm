#lang sicp

(#%require rackunit)

(define (tree-map proc items)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       items))

(define (square-tree3 items)
  (tree-map square items))

;; 2.30
(define (square x) (* x x))

(define (square-tree2 items)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree2 sub-tree)
             (square sub-tree)))
       items))

(check-equal? (square-tree3 '(1 (2 (3 4) 5))) (square-tree2 '(1 (2 (3 4) 5))))
