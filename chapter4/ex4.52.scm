(define (if-fail? exp) (tagged-list? exp 'if-fail))

(define (if-fail-first exp) (cadr exp))

(define (if-fail-second exp) (caddr exp))

(define (analyze-if-fail exp)
  (let ((first (analyze (if-fail-first exp)))
        (second (analyze (if-fail-second exp))))
    (lambda (env succeed fail)
      (first env
             (lambda (first-value fail2)
               (succeed 'ok fail2))
             (lambda ()
               (second env succeed fail))))))

;; test
(if-fail (let ((x (an-element-of '(1 3 5 8 10))))
           (require (even? x))
           x)
         'all-odd)