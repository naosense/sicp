#lang sicp

(#%require "text.scm")

(#%provide (all-defined))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (if (leaf? tree)
      '()
      (let ((left (left-branch tree))
            (right (right-branch tree)))
        (cond ((null? symbol) '())
              ((element-of-set? symbol (symbols left))
               (cons '0 (encode-symbol symbol left)))
              ((element-of-set? symbol (symbols right))
               (cons '1 (encode-symbol symbol right)))
              (else (error "bad symbol" symbol))))))

;;(display (encode '(A D A B B C A) sample-tree))
      