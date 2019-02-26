#lang sicp

(#%require "text.scm")

(#%provide (all-defined))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))


;; e.g. ((leaf D 1) (leaf C 1) (leaf B 2) (leaf A 4))
(define (successive-merge set)
  (if (= (length set) 1)
      (car set)
      (let ((first (car set))
            (second (cadr set))
            (remaining-set (cddr set)))
        (successive-merge (adjoin-set (make-code-tree first second)
                                      remaining-set)))))


;(display (generate-huffman-tree '((A 4) (C 1) (B 2) (D 1))))


