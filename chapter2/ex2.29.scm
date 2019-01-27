#lang sicp

(#%require rackunit)

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch m)
  (car m))

(define (right-branch m)
  (car (cdr m)))

(define (branch-length b)
  (car b))

(define (branch-structure b)
  (car (cdr b)))

;(define (total-weight m)
;  (let ((left-weight (branch-structure (left-branch m)))
;        (right-weight (branch-structure (right-branch m))))
;    (cond ((null? m) 0)
;          ((not (pair? left-weight))
;           (if (not (pair? right-weight))
;               (+ left-weight
;                  right-weight)
;               (+ left-weight
;                  (total-weight right-weight))))
;          ((not (pair? right-weight))
;           (if (not (pair? left-weight))
;               (+ left-weight
;                  right-weight)
;               (+ (total-weight left-weight)
;                  right-weight)))
;          (else
;           (+ (total-weight left-weight)
;              (total-weight right-weight))))))

(define (total-weight mobile) 
   (cond ((null? mobile) 0) 
         ((not (pair? mobile)) mobile) 
         (else (+ (total-weight (branch-structure (left-branch mobile))) 
                  (total-weight (branch-structure (right-branch mobile)))))))

(define (balanced? mobile)
  (define (torque branch) 
   (* (branch-length branch) (total-weight (branch-structure branch)))) 
   (if (not (pair? mobile)) 
       true 
       (and (= (torque (left-branch mobile)) (torque (right-branch mobile))) 
            (balanced? (branch-structure (left-branch mobile))) 
            (balanced? (branch-structure (right-branch mobile)))))) 


  
          
          
  

(total-weight (make-mobile (make-branch 7 (make-mobile (make-branch 2 5)
                                                       (make-branch 7 5)))
                           (make-branch 3 (make-mobile (make-branch 2 2)
                                                       (make-branch 7 1)))))

(balance? (make-mobile (make-branch 7 (make-mobile (make-branch 2 5)
                                                       (make-branch 7 5)))
                           (make-branch 3 (make-mobile (make-branch 2 2)
                                                       (make-branch 7 1)))))
