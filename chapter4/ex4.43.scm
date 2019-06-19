;; 知道Mary姓Moore
(define (name-puzzle-1)
  (let ((melissa 'Sir-Barnacle-Hood)
        (lorna (amb 'Colonel-Downing 'Mr-Hall 'Dr-Parker))
        (rosalind (amb 'Colonel-Downing 'Dr-Parker))
        (gabriella (amb 'Colonel-Downing 'Mr-Hall))
        (mary 'Mr-Moore))
    (let ((res (list (list 'melissa melissa)
                     (list 'lorna lorna)
                     (list 'rosalind rosalind)
                     (list 'gabriella gabriella)
                     (list 'mary mary))))
    
      (require (eq? (get-val gabriella name-pairs)
                    (get-key 'Dr-Parker res)))
      (require (distinct? (list melissa lorna rosalind gabriella mary)))
      res)))

;; 不知道Mary姓Moore
(define (name-puzzle-2)
  (let ((melissa 'Sir-Barnacle-Hood)
        (lorna (amb 'Colonel-Downing 'Mr-Hall 'Dr-Parker))
        (rosalind (amb 'Mr-Moore 'Colonel-Downing 'Dr-Parker))
        (gabriella (amb 'Mr-Moore 'Colonel-Downing 'Mr-Hall))
        (mary (amb 'Mr-Moore 'Colonel-Downing 'Mr-Hall)) ;; changed
        )
    (let ((res (list (list 'melissa melissa)
                     (list 'lorna lorna)
                     (list 'rosalind rosalind)
                     (list 'gabriella gabriella)
                     (list 'mary mary))))
    
      (require (eq? (get-val gabriella name-pairs)
                    (get-key 'Dr-Parker res)))
      (require (distinct? (list melissa lorna rosalind gabriella mary)))
      res)))

(define name-pairs
  (list (list 'Mr-Moore  'lorna)
        (list 'Colonel-Downing 'melissa)
        (list 'Mr-Hall 'rosalind)
        (list 'Sir-Barnacle-Hood 'gabriella)
        (list 'Dr-Parker 'mary)))

(define (caar c)
  (car (car c)))

(define (cadar c)
  (car (cdr (car c))))

(define (get-val key dict)
  (cond ((null? dict) '())
        ((eq? key (caar dict)) (cadar dict))
        (else (get-val key (cdr dict)))))

(define (get-key val dict)
  (cond ((null? dict) '())
        ((eq? val (cadar dict)) (caar dict))
        (else (get-key val (cdr dict)))))

(name-puzzle-2)