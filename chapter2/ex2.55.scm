#lang sicp

(#%require rackunit)

;; 类似于一种列表
(check-equal? (car ''abracadabra) 'quote)
(check-equal? (cadr ''abracadabra) 'abracadabra)