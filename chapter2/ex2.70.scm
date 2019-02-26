#lang sicp

(#%require "text.scm")
(#%require "ex2.68.scm")
(#%require "ex2.69.scm")

(define tree
  (generate-huffman-tree '((A 2) (NA 16) (BOOM 1) (SHA 3) (GET 2) (YIP 9) (JOB 2) (WAH 1))))

(define song '(GET A JOB SHA NA NA NA NA NA NA NA NA
                   GET A JOB SHA NA NA NA NA NA NA NA NA
                   WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
                   SHA BOOM))


(define code (encode song tree))

(display tree)
(newline)
(display (length code))
(newline)
(display (* 3 (length song)))