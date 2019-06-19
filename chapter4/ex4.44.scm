;; plain and straitforward solution
(define (eight-queens)
  (let ((r1 (amb 1 2 3 4 5 6 7 8)))
    (let ((r2 (amb 1 2 3 4 5 6 7 8)))
      (require (safe? r2 2 (rows->poses (list r1))))
      (let ((r3 (amb 1 2 3 4 5 6 7 8)))
        (require (safe? r3 3 (rows->poses (list r1 r2))))
        (let ((r4 (amb 1 2 3 4 5 6 7 8)))
          (require (safe? r4 4 (rows->poses (list r1 r2 r3))))
          (let ((r5 (amb 1 2 3 4 5 6 7 8)))
            (require (safe? r5 5 (rows->poses (list r1 r2 r3 r4))))
            (let ((r6 (amb 1 2 3 4 5 6 7 8)))
              (require (safe? r6 6 (rows->poses (list r1 r2 r3 r4 r5))))
              (let ((r7 (amb 1 2 3 4 5 6 7 8)))
                (require (safe? r7 7 (rows->poses (list r1 r2 r3 r4 r5 r6))))
                (let ((r8 (amb 1 2 3 4 5 6 7 8)))
                  (require (safe? r8 8 (rows->poses (list r1 r2 r3 r4 r5 r6 r7))))
                  (rows->poses (list r1 r2 r3 r4 r5 r6 r7 r8)))))))))))

;; helper functions
(define (and a b c d)
  (cond ((not a) false)
        ((not b) false)
        ((not c) false)
        ((not d) false)
        (else true)))

(define (or a b)
  (if a
      true
      b))

;; 2.42
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

(define (safe? row col positions)
  (define (safe-iter kp other-positions)
    (if (null? other-positions)
        true
        (and (not (same-row? kp (car other-positions)))
             (not (same-col? kp (car other-positions)))
             (not (same-diag? kp (car other-positions)))
             (safe-iter kp (cdr other-positions)))))
  (safe-iter (cons row col) positions))

(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (rows->poses rows)
  (define count 0)
  (map (lambda (row)
         (begin (set! count (+ count 1))
                (cons row count)))
       rows))

(eight-queens)
