#lang sicp

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

;; 列表不算序对吗
(count-pairs '(a b c))
(count-pairs '((a) (a)))
(count-pairs (cons (cons '(a) '(a)) (cons '(a) '(a))))