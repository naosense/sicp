(assert! (rule (reverse () ())))
(assert! (rule (reverse (?x . ?y) ?z)
               (and (append-to-form ?v (?x) ?z)
                    (reverse ?y ?v))))

;;; Query input:
(reverse ?x (1 2 3))

;;; Query results:
(reverse (3 2 1) (1 2 3))

;;; 死循环
;;; Query input:
(reverse (1 2 3) ?x)

;;; 会形成下面的计算，卡在这里
(append-to-form ?v (1) ?x)
