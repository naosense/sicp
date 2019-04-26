#lang sicp

(#%provide (all-defined))

(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))

;; (withdraw 25)
;; (withdraw 25)
;; (withdraw 60)

;;  将balance限制到局部变量
(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

;; (new-withdraw 25)
;; (new-withdraw 25)
;; (new-withdraw 60)

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

;; (define W1 (make-withdraw 100))
;; (define W2 (make-withdraw 100))
;; (W1 50)
;; (W2 70)

(define (make-account balance)
  ; 在这里定义和在外面定义对环境的影响
  ; 如果在外面定义balance就是全部环境中的balance，
  ; 而如果在这里定义balance就是make-account运行环境中的balance
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

;; (define acc (make-account 100))
;; ((acc 'withdraw) 50)
;; ((acc 'withdraw) 60)
;; ((acc 'deposit) 40)
;; ((acc 'withdraw) 60)

(define rand
  (lambda () (random 4294967087)))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

;; (estimate-pi 1000000)

; 函数运行环境的外围环境(父环境)是创建函数的环境,测试程序:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define xx 10)

(define (global-f)
  xx)

(define (env-test)
  (define (local-f) xx)
  (define xx 1)
  (cons (global-f) (local-f)))

;; (global-f)
;; (env-test)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(define x (list 'a 'b))
;;(define z1 (cons x x))
;;(define z2 (cons (list 'a 'b) (list 'a 'b)))
; 队列
(define (front-ptr queue) (car queue))

(define (rear-ptr queue) (cdr queue))

(define (set-front-ptr! queue item)
  (set-car! queue item))

(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))

;; 一维表格
(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) (cdr table)))))
  'ok)

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (make-table)
  (list '*table*))

(define t1 (make-table))
;; (insert! 'a 1 t1)
;; t1
;; (insert! 'b 2 t1)
;; t1

;;  二维表格
;; (define (lookup key-1 key-2 table)
;;   (let ((subtable (assoc key-1 (cdr table))))
;;     (if subtable
;;         (let ((record (assoc key-2 (cdr subtable))))
;;           (if record
;;               (cdr record)
;;               false))
;;         false)))
;;
;; (define (insert! key-1 key-2 value table)
;;   (let ((subtable (assoc key-1 (cdr table))))
;;     (if subtable
;;         (let ((record (assoc key-2 (cdr subtable))))
;;           (if record
;;              (set-cdr! record value)
;;              (set-cdr! subtable
;;                   (cons (cons key-2 value) (cdr subtable)))))
;;         (set-cdr! table
;;                   (cons ;;(cons key-1 (list (cons key-2 value)))亦可
;;                         (list key-1 (cons key-2 value))
;;                         (cdr table)))))
;;   'ok)
;;
;; (insert! 'math '+ 43 t1)
;; (insert! 'math '- 45 t1)
;; (insert! 'letters 'a 97 t1)
;; (insert! 'letters 'b 98 t1)
;; (lookup 'math '- t1)
;; (lookup 'letters 'b t1)

;; 数字电路模拟器
(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-and a1 a2)
  (if (and (= a1 1) (= a2 1))
      1
      0))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value (logical-and (get-signal a1)
                                  (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

;;;; ex3.28
(define (logical-or a1 a2)
  (if (or (= a1 1) (= a2 1))
      1
      0))

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value (logical-or (get-signal a1)
                                 (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(define (make-wire)
  (let ((signal-value 0) (action-procedure '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedure))
          'done))

    (define (accept-action-procedure! proc)
      (set! action-procedure (cons proc action-procedure))
      (proc))

    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknow operation -- WIRE" m))))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin
        ((car procedures))
        (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

;; 待处理表，用于delay处理
(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))
;; 驱动事件处理
;; FIFO
(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))
;; LIFO
;; (define (propagate)
;;   (if (empty-agenda? the-agenda)
;;       'done
;;       (let ((first-item (first-agenda-item the-agenda)))
;;         (remove-first-agenda-item! the-agenda)
;;         (propagate)
;;         (first-item))))

;; 探针程序
;;(define (probe name wire)
;;  (add-action! wire
;;               (lambda ()
;;                 (newline)
;;                 (display name)
;;                 (display " ")
;;                 (display (current-time the-agenda))
;;                 (display "  New-value = ")
;;                 (display (get-signal wire)))))

(define (make-time-segment time queue)
  (cons time queue))

(define (segment-time s) (car s))

(define (segment-queue s) (cdr s))

(define (make-agenda) (list 0))

(define (current-time agenda) (car agenda))

(define (set-current-time! agenda time)
  (set-car! agenda time))

(define (segments agenda) (cdr agenda))

(define (set-segments! agenda segments)
  (set-cdr! agenda segments))

(define (first-segment agenda) (car (segments agenda)))

(define (rest-segments agenda) (cdr (segments agenda)))

(define (empty-agenda? agenda) (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))

  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))

  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments))
                       action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr!
               segments
               (cons (make-new-time-segment time action)
                     ;; 换成rest行不？
                     (cdr segments)))
              (add-to-segments! rest)))))

  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
         agenda
         (cons (make-new-time-segment time action)
               segments))
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty --FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))
;; (probe 'sum sum)
;; (probe 'carry carry)

;; (half-adder input-1 input-2 sum carry)
;; (set-signal! input-1 1)
;; (propagate)
;; (set-signal! input-2 1)
;; (propagate)

;; 约束传播

(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       me))
          ((and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       me))
          ((and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       me))))

  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))

  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- ADDER" request))))

  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))

(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1) (get-value m2))
                       me))
          ((and (has-value? m1) (has-value? product))
           (set-value! m2
                       (/ (get-value product) (get-value m1))
                       me))
          ((and (has-value? m2) (has-value? product))
           (set-value! m1
                       (/ (get-value product) (get-value m2))
                       me))))

  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))

  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- MULTIPLIER" request))))

  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (constant value connector)
  (define (me request)
    (error "Unknown request -- CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

(define (probe name connector)
  (define (print-probe value)
    (newline)
    (display name)
    (display " = ")
    (display value))

  (define (process-new-value)
    (print-probe (get-value connector)))

  (define (process-forget-value)
    (print-probe "?"))

  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- PROBE" request))))
  (connect connector me)
  me)

(define (make-connector)
  (let ((value false) (informant false) (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (error "Contradiction" (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant false)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))
          'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints
                (cons new-constraint constraints)))
      (if (has-value? me)
          (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant true false))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Unknown operation -- CONNECTOR"
                         request))))
    me))

(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))

(define (has-value? connector)
  (connector 'has-value?))

(define (get-value connector)
  (connector 'value))

(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))

(define (forget-value! connector retractor)
  ((connector 'forget) retractor))

(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

(define C (make-connector))
(define F (make-connector))
(define (celsius-fahrenheit-converter c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))
;;(celsius-fahrenheit-converter C F)
;;(probe "Celsius temp" C)
;;(probe "Fahrenheit temp" F)
;;(set-value! C 25 'user)
;;(forget-value! C 'user)
;;(set-value! F 212 'user)

;; 并发的本质

;; 流处理
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

;;(define (stream-map proc s)
;;  (if (stream-null? s)
;;      the-empty-stream
;;      (cons-stream (proc (stream-car s))
;;                   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-stream-n s n)
  (display-one-line (stream-car s))
  (if (= n 0)
      'done
      (display-stream-n (stream-cdr s) (- n 1))))

(define (display-one-line x)
  (display x) (display " "))

(define (display-line x)
  (newline)
  (display x))

;; 注意这里的delay实现和课本不一样,
;; 至于为什么这么实现：
;; Be sure to observe that force can be implemented by a function,
;; whereas delay cannot. The reason is, of course,
;; that we cannot allow a functional implementation
;; of delay to evaluate the parameter of delay.
;; The whole point of delay is to avoid such evaluation.
;; This rules out an implementation of delay as a function.
;; The force primitive, on the other hand,
;; can be implemented by a function,
;; because it works on the value of a lambda expression.
;; 参考：http://people.cs.aau.dk/~normark/prog3-03/html/notes/
;; eval-order_themes-delay-stream-section.html#eval-order_delay-force_title_1
(define-syntax delay
  (syntax-rules ()
    ((delay expr)
     (memo-proc
      (lambda ()
        expr)))))

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a b)
     (cons a (delay b)))))

(define (stream-car s)
  (car s))

(define (stream-cdr s)
  (force (cdr s)))

;; (define (delay exp)
;;   (memo-proc (lambda () exp)))

(define (force delayed-object)
  (delayed-object))

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

;; (display-stream (stream-enumerate-interval 1 10))
;; (stream-ref (stream-enumerate-interval 1 10) 0)

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

;; (define integers (integers-starting-from 1))

(define (divisible? x y) (= (remainder x y) 0))
;;
;; (define no-sevens
;;   (stream-filter (lambda (x) (not (divisible? x 7)))
;;                  integers))
;;
;; (define (fibgen a b)
;;   (cons-stream a (fibgen b (+ a b))))
;;
;; (define fibs (fibgen 0 1))
;;
;; (define (sieve stream)
;;   (cons-stream
;;    (stream-car stream)
;;    (sieve (stream-filter
;;            (lambda (x)
;;              (not (divisible? x (stream-car stream))))
;;            (stream-cdr stream)))))
;;
;; (define primes (sieve (integers-starting-from 2)))

;; (stream-ref integers 100)
;; (stream-ref no-sevens 100)
;; (stream-ref fibs 100)
;; (display-stream-n integers 10)
;; (display-stream-n fibs 10)
;; (display-stream-n primes 10)

;; 隐式定义流
(define ones (cons-stream 1 ones))

(define (stream-map proc . argsstreams)
  (if (stream-null? (car argsstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argsstreams))
       (apply stream-map
              (cons proc (map stream-cdr argsstreams))))))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define integers (cons-stream 1 (add-streams ones integers)))

(define fibs
  (cons-stream 0
               (cons-stream 1
                            (add-streams
                             (stream-cdr fibs)
                             fibs))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define double (cons-stream 1 (scale-stream double 2)))

(define pi (* 4 (atan 1.0)))

;; (display-stream-n integers 10)
;; (display-stream-n fibs 10)
;; (display-stream-n double 10)

;; 将迭代过程表示为流
(define (sqrt-stream x)
  (define (average a b)
    (/ (+ a b) 2))
  (define (sqrt-improve guess x)
    (average guess (/ x guess)))
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

;; ex3.55
(define (partial-sums s)
  (let ((s0 (stream-car s))
        (s1 (stream-cdr s)))
    (define accu
      (cons-stream s0 (add-streams s1 accu)))
    accu))

(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               ;; 这里还可以用scale-stream -1
               (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

(define (square x) (* x x))

;; 加速器
(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(define (accelerated-sequence transform s)
  (stream-map  stream-car
               (make-tableau transform s)))

;; (display-stream-n (sqrt-stream 2) 10)
;; (display-stream-n pi-stream 10)
;; (display-stream-n (euler-transform pi-stream) 10)
;; (display-stream-n (accelerated-sequence euler-transform pi-stream) 10)

;; 序对的无穷流
;; 注意这个针对无穷流时，s2永远出不来
(define (stream-append s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (stream-append (stream-cdr s1) s2))))

;; 所以使用交错取两个流的元素
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

;; (display-stream-n (interleave integers ones) 10)
(define (pairs s t)
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (stream-map (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t)))))

;; (display-stream-n (pairs integers integers) 10)
