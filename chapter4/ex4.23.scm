;; (lambda ()
;;   proc1
;;   proc2
;;   proc3)
;; 原生版本将会转化为
;; (lambda (env)
;;   ((lambda (env)
;;      ((lambda (env) (proc1 env) (proc2 env)) env)
;;      (proc3 env))
;;    env))
;; 而本题版本将会转化为
;; (lambda (env)
;;   (proc1 env)
;;   (execute-sequence ((proc2) (proc3)) env))
;; 也就是一个是分析的阶段就会完全展开，而另一个会等到
;; 运行时才会展开
