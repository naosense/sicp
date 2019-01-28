#lang sicp

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        ;; s的集合等于去掉第一个元素剩余元素的集合加上第一个元素与剩余元素集合的每一个元素的拼接
        ;; 具体到(1 2 3)的集合等于(2 3)的集合加上1与(2 3)集合每一个元素的拼接
        (append rest (map (lambda (x) (cons (car s) x))
                          rest)))))

(display (subsets '(1 2 3)))
                            