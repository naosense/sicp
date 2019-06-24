#lang sicp

;; 因为person1和person2互为彼此
;; 可以对人名强制一个顺序
(define (compare person1 person2)
  (define (symbols->strings symbols)
    (map (lambda (symbol)
           (symbol->string symbol))
         symbols))
  (define (iter p1 p2)
    (cond ((and (null? p1) (null? p2)) 0)
          ((null? p1) -1)
          ((null? p2) 1)
          ((equal? (car p1) (car p2))
           (iter (cdr p1) (cdr p2)))
          ((string<? (car p1) (car p2)) -1)
          (else 1)))
  (iter (symbols->strings person1) (symbols->strings person2)))

(compare '(a b c) '(c d e))
(compare '(a b c) '(a b c))
(compare '(a b d) '(a b c))

;; 升序排列
(and (lives-near ?person1 person2)
     (lisp-value (lambda (p1 p2)
                   (= (compare p1 p2) -1))
                 ?person1 ?person2))
