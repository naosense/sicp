#lang sicp

(#%require rackunit)

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define x '(a b))
(define y '(c d))

(define z (append x y))
(check-equal? z '(a b c d))
(check-equal? (cdr x) '(b))

(define w (append! x y))
(check-equal? w '(a b c d))
(check-equal? (cdr x) '(b c d))

;(set-cdr! y 'e)
z
w

;;                 -> x: [a,]--->[b,]--
;;                 |      ____________|  
;;                 |      | 
;;                 |  y: [c,]--->[d,nil]
;;                 |      ^ 
;;                 |      |
;; z: [a,]--->[b,]-|-------
;; w:---------------