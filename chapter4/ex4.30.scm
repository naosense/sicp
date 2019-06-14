a) display是原始函数，对参数严格

(define (p1 x)
  (set! x (cons x '(2)))
  x)

(define (p2 x)
  (define (p e)
    e
    x)
  (p (set! x (cons x '(2)))))

b) 书本：(p1 1) -> (1 . 2)，(p2 1) -> 1 e:(set! x (cons x '(2)))最终会执行到
         lookup-variable-value，返回一个延迟对象
    cy ：(p1 1) -> (1 . 2)，(p2 1) -> (1 . 2)，会调用actual-value进而强迫求值

(define (for-each proc items)
  (if (null? items)
      'done
      (begin (proc (car items))
             (for-each proc (cdr items)))))

(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))

c) 因为a中序列的每个表达式本来就会求值

d) 各有利弊，所以可以将权利交给程序员
