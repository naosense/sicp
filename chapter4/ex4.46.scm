;; 拿the student with the cat sleeps in the class这句话，
;; 如果是从右往左运算首先匹配到一个class后，
;; (set! *unparsed* (cdr *unparsed*))
;; *unparsed*为空，无法匹配前面的内容了
