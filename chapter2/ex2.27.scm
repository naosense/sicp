#lang sicp

(#%require rackunit)

(define (deep-reverse l)
  (cond ((null? l) nil)
        ((not (pair? (car l)))
         (append (deep-reverse (cdr l)) (list (car l))))
        (else
         (append (deep-reverse (cdr l))
                 ;; (append '(4 3) '(2 1) = (4 3 2 1)
                 ;; (append '((4 3)) '((2 1)) = ((4 3) (2 1))
                 ;; 可以看到这里的list和上面分支的list有严格的对称美
                 (list (deep-reverse (car l)))))))


(check-equal? (deep-reverse '((5 (1 2)) (3 4))) '((4 3) ((2 1) 5)))
         
