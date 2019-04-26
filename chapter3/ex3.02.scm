#lang sicp

(#%provide (all-defined))

(define (make-monitored f)
  (let ((count 0))
    (lambda (sym)
      (cond ((eq? sym 'how-many-calls) count)
            ((eq? sym 'reset-count) (set! count 0) count)
            (else (set! count (+ count 1)) (f sym))))))

;(define s (make-monitored sqrt))
;(s 100)
;(s 'how-many-calls)
;(s 110)
;(s 'how-many-calls)
;(s 'reset-count)
;(s 10)
;(s 'how-many-calls)