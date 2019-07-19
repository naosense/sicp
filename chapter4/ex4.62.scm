#lang sicp

(assert! (rule (last-pair (?x . ()) (?x))))
(assert! (rule (last-pair (?x . ?y) (?z))
               (last-pair ?y (?z))))

;; 对于(last-pair ?x (3))不能正常工作，貌似陷入了死循环，原因待查
