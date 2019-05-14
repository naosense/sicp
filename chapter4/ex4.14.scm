#lang sicp
;; 如果作为primitive函数，将有
;; (apply map ((procedure (x)                                      ;; parameter
;;                        ((* x x))                                ;; body
;;                        (((false true car cdr cons null? * map)  ;; env
;;                          #f #t (primitive #<procedure:mcar>) 
;;                          (primitive #<procedure:mcdr>) 
;;                          (primitive #<procedure:mcons>) 
;;                          (primitive #<procedure:null?>) 
;;                          (primitive #<procedure:*>) 
;;                          (primitive #<procedure:mmap>)))) 
;;             (1 2 3)))

;; 而正确的形式应该为
(apply map (list (lambda (x) (* x x)) '(1 2 3)))


;; 注意着四行的区别与联系，list会提前运算，'会延时
;; (car (list map (lambda (x) (* x x)) '(1 2 3)))
;; (car '(map (lambda (x) (* x x)) '(1 2 3)))
;; (list 1 2 3)
;; '(1 2 3)
