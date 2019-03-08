; a) number? variable?没有数据标志
; b)
; c)
(define (install-deriv-package)
  (put 'deriv '+ (lambda (exp var)
                   (make-sum (deriv (addend exp) var)
                             (deriv (augend exp) var))))
  (put 'deriv '* (lambda (exp var)
                   (make-sum
                    (make-product (multiplier exp)
                                  (deriv (multiplicand exp) var))
                    (make-product (deriv (multiplier exp) var)
                                  (multiplicand exp)))))
  (put 'deriv '** (lambda (exp var)
                    (let ((n (exponent exp))
                          (u (base exp)))
                      (make-product n
                                    (make-product (deriv u var)
                                                  (make-exponentiation u (- n 1))))))))
; d)
; 只需要改变下put参数顺序
