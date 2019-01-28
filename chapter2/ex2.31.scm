#lang sicp

(#%require rackunit)
(#%require "../chapter1/text.scm")
(#%require "ex2.30.scm")

(define (tree-map proc items)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       items))

(define (square-tree3 items)
  (tree-map square items))

(check-equal? (square-tree3 '(1 (2 (3 4) 5))) (square-tree2 '(1 (2 (3 4) 5))))