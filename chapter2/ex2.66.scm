#lang sicp

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((< given-key (key (car set-of-records)))
         (lookup given-key (left-branch set-of-records)))
        ((> given-key (key (car set-of-records)))
         (lookup given-key (right-branch set-of-records)))
        ((= given-key (key (car set-of-records)))
         (entry set-of-records))))