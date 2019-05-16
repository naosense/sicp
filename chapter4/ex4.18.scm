;; 如果按照4.16的方法将被转化为
(lambda (f y0 dt)
  (let ((y *unassigned*)
        (dy *unassigned*))
    (set y (integral (delay dy) y0 dt))
    (set dy (stream-map f y))
    y))
;; 如果按照本题的方法将会转化为
(lambda (f y0 dt)
  (let ((y *unassigned*)
        (dy *unassigned*))
    (let ((a (integral (delay dy) y0 dt))
          (b (stream-map f y)))
      (set y a)
      (set dy b)
      y)))
;; 第一种可行，第二种不可行，因为对于integral的第一个元素是y0,
;; dy可以拿到y的第一个元素进而生成自己的第一个元素，而第二种
;; 当计算b (stream-map f y)时，y还为*unassigned*，无法像
;; 第一种那样交替计算出y和dy