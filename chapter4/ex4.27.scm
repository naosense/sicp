;; 依次是
;; count
;; 1
;; w
;; 10
;; count
;; 2
;; 关键一点是第一次获取count时为啥是1
;; 根据代码执行的路径可以基本得出如下流程
;; id会make-lambda -> make-procedure形成一个过程
;; w为(id (id 10))最终会执行(set! count (+ count 1))
;; 然后将(delay-it (id 10))赋值给w，这也就是第一步
;; count为1的原因，因为作为id形式参数的x现在的值为
;; (id 10)，而id作为一个复合函数对参数是非严格的，
;; 那么会对参数进行延迟，w的返回值正好是这个x，
;; 所以w为这个延迟对象
;; 第二步输入w，将会在drive-loop执行(actual-value w)，延迟对象
;; 要求值，count+1=2，而实际值同样等于形参x=10
