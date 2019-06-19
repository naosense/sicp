#lang sicp

(#%require rackunit)

(define (square x) (* x x))

(define (square-tree1 items)
  (cond ((null? items) nil)
        ((not (pair? (car items)))
         (cons (square (car items)) (square-tree1 (cdr items))))
        (else
         (cons (square-tree1 (car items)) (square-tree1 (cdr items))))))


(define (square-tree2 items)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree2 sub-tree)
             (square sub-tree)))
       items))

(check-equal? (square-tree1 '(1 (2 (3 4) 5))) (square-tree2 '(1 (2 (3 4) 5))))
