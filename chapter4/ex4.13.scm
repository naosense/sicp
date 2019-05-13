#lang sicp

;; 这里是删除外围环境中的变量var，有待商榷
(define (make-unbound! var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set! vars (cdr vars))
             (set! vals (cdr vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (not (eq? env the-empty-environment))
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))
