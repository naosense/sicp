#lang sicp

(#%require "text.scm")

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board '())

(define (adjoin-position row col rest)
  (cons (cons row col) rest))

(define (same-row? p1 p2)
  (= (car p1) (car p2)))

(define (same-col? p1 p2)
  (= (cdr p1) (cdr p2)))

(define (same-diag? p1 p2)
  (let ((row1 (car p1))
        (col1 (cdr p1))
        (row2 (car p2))
        (col2 (cdr p2)))
    (or (= (+ row1 col1) (+ row2 col2))
        (= (- row1 col1) (- row2 col2)))))

(define (safe? k positions)
  (define (safe-iter kp other-positions)
    (if (null? other-positions)
        true
        (and (not (same-row? kp (car other-positions)))
             (not (same-col? kp (car other-positions)))
             (not (same-diag? kp (car other-positions)))
             (safe-iter kp (cdr other-positions)))))
  (let ((k-position (car (filter (lambda(p) (= (cdr p) k)) positions)))
        (other-positions (filter (lambda(p) (not (= (cdr p) k))) positions)))
    (safe-iter k-position other-positions)))

(display (queens 8))
      
    
  

