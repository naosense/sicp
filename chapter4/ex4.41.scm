;; 另外还可以用第二章的permutation，filter模式

(define (for-each proc items)
  (if (null? items)
      'done
      (begin (proc (car items))
             (for-each proc (cdr items)))))

(for-each
 (lambda (miller)
   (for-each
    (lambda (cooper)
      (if (> miller cooper)
          (for-each
           (lambda (fletcher)
             (if (not (= (abs (- fletcher cooper)) 1))
                 (for-each
                  (lambda (smith)
                    (if (not (= (abs (- smith fletcher)) 1))
                        (for-each
                         (lambda (baker)
                           (if (distinct? (list baker cooper fletcher miller smith))
                               (display (list (list 'baker baker)
                                              (list 'cooper cooper)
                                              (list 'fletcher fletcher)
                                              (list 'miller miller)
                                              (list 'smith smith)))))
                         '(1 2 3 4))))
                  '(1 2 3 4 5))))
           '(2 3 4))))
    '(2 3 4 5)))
 '(1 2 3 4 5))
