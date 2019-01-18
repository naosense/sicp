#lang sicp

(#%require rackunit)

;; 使用递归过程计算帕斯卡三角
;;         1
;;       1   1
;;     1   2   1
;;   1   3   3   1
;; 1   4   6   4   1

(define (pascal-triangle row col)
  (cond ((< row 3) 1)
        ((or (= col 1) (= col row)) 1)
        (else (+ (pascal-triangle (- row 1) (- col 1))
                 (pascal-triangle (- row 1) col)))))

(check-eq? (pascal-triangle 1 1) 1)
(check-eq? (pascal-triangle 3 2) 2)
(check-eq? (pascal-triangle 5 1) 1)
(check-eq? (pascal-triangle 5 2) 4)
(check-eq? (pascal-triangle 5 3) 6)
(check-eq? (pascal-triangle 5 5) 1)