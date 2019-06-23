(define count 0)

(let ((x (an-element-of '(a b c)))
      (y (an-element-of '(a b c))))
  (set! count (+ count 1))
  (require (not (eq? x y)))
  (list x y count))

;; solution，只要把set!的下面代码注释掉就可以了
(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (let ((old-value
                      (lookup-variable-value var env)))
                 (set-variable-value! var val env)
                 (succeed 'ok
                          (lambda ()
                           ; (set-variable-value! var
                           ;                      old-value
                           ;                      env)
                            (fail2)))))
             fail))))
