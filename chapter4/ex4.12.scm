#lang sicp

(define (iter env var fail-callback success-callback)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars) (fail-callback))
            ((eq? var (car vars))
             (success-callback vars vals))
            (else
              (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (iter env
              var
              (lambda () (env-loop (enclosing-environment env)))
              (lambda (vars vals) (car vals)))))
  (env-loop env))

(define (define-variable! var val env)
  (iter env
        var
        (lambda () (add-binding-to-frame! var val (first-frame env)))
        (lambda (vars vals) (set-car! vals val))))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (iter env
              var
              (lambda () (env-loop (enclosing-environment env)))
              (lambda (vars vals) (set-car! vals val)))))
  (env-loop env))
