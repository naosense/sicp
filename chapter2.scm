; performance measure
(define-syntax time
  (syntax-rules ()
    ((_ s)
     (with-timings
	 (lambda () s)
       (lambda (run-time gc-time real-time)
	 (write (internal-time/ticks->seconds run-time))
	 (write-char #\space)
	 (write (internal-time/ticks->seconds gc-time))
	 (write-char #\space)
	 (write (internal-time/ticks->seconds real-time))
	 (newline))))))

(define (make-leaf symbol weight)
  (list `leaf symbol weight))

(define (leaf? object)
  (eq? (car object) `leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

; cdr返回的是一个list，即使里面只有一个元素，所以还需要car一下
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
    (weight-leaf tree)
    (cadddr tree)))

; decode huffman tree
; (define (decode bits tree)
;   (define (decode-1 bits current-branch)
;     (if (null? bits)
;       '()
;       (let (next-branch
;              (choose-branch (car bits) current-branch)))
;       (if (leaf? next-branch)
;         (cons (symbol-leaf next-branch)
; 

