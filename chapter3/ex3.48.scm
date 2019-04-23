#lang sicp

;; 这里实际上是避免了互锁，即A向B转账的同时B向A转账，
;; 这种场景下A，B既是account1又是account2，如果不能
;; 依赖某种特征取得某种顺序，就会发生一个线程获得了A
;; 上的锁，请求B上的锁，同时另一个线程获得了B上的锁，
;; 请求A上的锁，双方僵持不下，谁也无法继续执行
(define (serialized-exchange account1 account2) 
  (let ((serializer1 'serializer-for-bigger-id--acc) 
        (serializer2 'serializer-for-smaller-id-acc)) 
    (cond ((> (get-id account1) (get-id account2)) 
           (set! serializer1 (account1 'serializer)) 
           (set! serializer2 (account2 'serializer))) 
          (else (set! serializer1 (account2 'serializer)) 
                (set! serializer2 (account1 'serializer)))) 
    ((serializer1 (serializer2 exchange)) account1 account2))) 