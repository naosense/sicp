#lang sicp

;; 从左往右，从右往左的把first和rest调换即可
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      ;; 这里不能只用一个let，同一层级变量的计算也是受cons运算
      ;; 顺序影响的
      (let ((first (eval (first-operand exps) env)))
        (let ((rest (list-of-values (rest-operands exps) env)))
          (cons first rest)))))
