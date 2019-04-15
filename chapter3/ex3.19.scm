#lang sicp

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define z (make-cycle '(a b c)))

;; 方法也就是所谓的快慢指针法，比如快指针一次走两步，慢指针一次走一步
;; 那么如果有环的话，快指针一定能够追上慢指针也就是快慢指针相等，且不为nil
(define (cycle? x)
  (define (single x)
    (if (pair? x)
        (cdr x)
        '()))
  (define (double x)
    (single (single x)))
  (let ((slow x)
        (fast x))
    (define (iter x)
      (set! slow (single slow))
      (set! fast (double fast))
      (cond ((not (pair? x)) false)
            ((null? (cdr x)) false) 
            ((equal? slow fast) true) 
            (else (iter (cdr x)))))
    (iter x))) 

(cycle? z)
(cycle? '())
(cycle? '(a))
(cycle? (make-cycle '(a)))
(cycle? '(a b c))