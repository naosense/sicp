;; 如果(halt? try try)返回true，也就是try对try是终止的，但(try try)将会永远运行下去，即不是终止的
;; 如果(halt? try try)返回false，也就是try对try不是终止的，但(try try)将会返回'halt即终止的，所以
;; 无论(halt? try try)返回什么，(try try)与返回值都是矛盾的
