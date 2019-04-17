#lang sicp



(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) false)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records)))))
    
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))

    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value) (cdr subtable)))))
            (set-cdr! local-table
                      (cons ;;(cons key-1 (list (cons key-2 value)))亦可
                       (list key-1 (cons key-2 value))
                       (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            (else (error "Unknow operation --TABLE" m))))
    dispatch))

(define t (make-table (lambda (k1 k2) (< (abs (- k1 k2)) 2))))

((t 'insert!) 1 2 'a)
((t 'insert!) 3 7 'b)
((t 'lookup) 0.5 1)
((t 'lookup) 0.5 0)
((t 'lookup) 2.7 7)
((t 'lookup) 1 7)
    