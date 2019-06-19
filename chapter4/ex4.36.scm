;; 如果使用an-integer-starting-from替换，将会导致无穷遍历
;; 比如，
;; i: (an-integer-starting-from 1)
;; j: (an-integer-starting-from i)
;; k: (an-integer-starting-from j)
;; 假设一开始i取1，j取1，k呢取值范围为1...∞，
;; 这样呢k永远尝试不完，也就没法向上返回
(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

(define (an-integer-between low high)
  (require (<= low high))
  (amb low (an-integer-between (+ low 1) high)))

(define (a-pythagorean-triple-from low)
  (let ((k (an-integer-starting-from low)))
    (let ((j (an-integer-between low k)))
      (let ((i (an-integer-between low j)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))
