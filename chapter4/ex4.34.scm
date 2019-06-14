#lang sicp

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp env))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((let? exp) (eval (let->combination exp) env))
        ((letrec? exp) (eval (letrec->let exp) env))
        ((application? exp)
         (meta-apply (actual-value (operator exp) env)
                     (operands exp)
                     env))
        (else
          (error "Unknown expression type -- EVAL" exp))))

;; (define (eval exp env)
;;   ((analyze exp) env))

;; 惰性求值

(define (delay-it exp env)
  (list 'thunk exp env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (thunk-exp thunk) (cadr thunk))

(define (thunk-env thunk) (caddr thunk))

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))

(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value
                         (thunk-exp obj)
                         (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result)
           (set-cdr! (cdr obj) '())
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))

(define (actual-value exp env)
  (force-it (eval exp env)))

(define apply-in-underlying-scheme apply)

;; 重命名了下apply
(define (meta-apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         ;;(apply-primitive-procedure procedure arguments))
         (apply-primitive-procedure
           procedure
           (list-of-arg-values arguments env)))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             ;;arguments
             (list-of-delayed-args arguments env)
             (procedure-environment procedure))))
        (else
          (error "Unknown procedure type -- APPLY" procedure))))

;; eval使用它生成实际参数表
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps)
                                env))))

(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it (first-operand exps) env)
            (list-of-delayed-args (rest-operands exps)
                                  env))))

;; 条件
;; (define (eval-if exp env)
;;   (if (true? (eval (if-predicate exp) env))
;;       (eval (if-consequent exp) env)
;;       (eval (if-alternative exp) env)))
(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

;; 序列 begin
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else
          (eval (first-exp exps) env)
          (eval-sequence (rest-exps exps) env))))

;; 赋值和定义
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

;; 自求值表达式只有数和字符串
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

;; 变量用符号表示
(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

;; text-of-quotation作如下修改
(define (text-of-quotation exp env)
  (let ((promise (cadr exp)))
    (if (pair? promise)
        (eval (literal->list promise) env)
        promise)))

;; 注意里面的几个quote使用
(define (literal->list exp)
  (if (null? exp)
      (list 'quote '())
      (list 'cons
            (list 'quote (car exp))
            (literal->list (cdr exp)))))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

;; 赋值
(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

;; 定义
(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

;; lambda表达式
(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;; if
(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

;; begin
(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq)
  (cons 'begin seq))

;; 过程应用，不属于上面的任意复合表达式
(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

;; 派生表达式，也就是通过已有的表达式表示的表达式
(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND-IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

;; ex4.08
(define (let? exp) (tagged-list? exp 'let))

(define (let-locals exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (cadr exp)))

(define (let-variable locals)
  (if (null? locals)
      '()
      (cons (caar locals) (let-variable (cdr locals)))))

(define (let-value locals)
  (if (null? locals)
      '()
      (cons (cadar locals) (let-value (cdr locals)))))

(define (let-body exp)
  (if (symbol? (cadr exp))
      (cdddr exp)
      (cddr exp)))

(define (let->combination exp)
  (let ((f (cadr exp))
        (locals (let-locals exp))
        (body (let-body exp)))
    (if (symbol? f)
        (make-begin (list (eval-definition f
                                           (make-lambda (let-variable locals) body))
                          (cons f (let-value locals))))
        (cons (make-lambda (let-variable locals) body)
              (let-value locals)))))

(define (make-let locals body)
  (cons 'let (cons locals body)))

;; ex4.20
(define (letrec? exp) (tagged-list? exp 'letrec))

(define (letrec-variables exp)
  (map car (cadr exp)))

(define (letrec-values exp)
  (map cadr (cadr exp)))

(define (letrec-body exp)
  (cddr exp))

(define (letrec->let exp)
  (let ((vars (letrec-variables exp))
        (vals (letrec-values exp))
        (body (letrec-body exp)))
    ;; 比较纳闷的是这里为什么要两个引号才行，其他地方一个就可以？
    (make-let (map (lambda (var) (list var ''*unassigned*)) vars)
              (append (map (lambda (var val) (list 'set! var val)) vars vals)
                      body))))

;; 注意这里的定义是所有不是false的都是true，
;; 而不是等于true的才是true，这意味着'a,1
;; 等等都是true
(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

;; 将给定的过程proc应用于args
;; (apply-primitive-procedure proc args)

;; 复合过程是由形式参数、过程体和环境构成
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))

(define (procedure-body p) (caddr p))

(define (procedure-environment p) (cadddr p))

;; 对环境操作
;; (lookup-variable-value var env)
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (if (eq? (car vals) '*unassigned*)
                 (error "Unassigned value" (car vars))
                 (car vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguements supplied" vars vals)
          (error "Too few arguements supplied" vars vals))))

;; (define-variable! var value env)
(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

;; (set-variable-value! var val env)
(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

;; 环境就是框架的表
(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

;; 框架就是一对表的序对
(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))

(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

;; 作为程序运行这个求值器
(define (setup-environment)
  (let ((initial-env
          (extend-environment (primitive-procedure-names)
                              (primitive-procedure-objects)
                              the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    (init-lazy-pair-funcs initial-env) ;; changed
    initial-env))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'list list)
        (list 'null? null?)
        (list 'pair? pair?)
        (list 'eq? eq?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '= =)
        (list 'runtime runtime)
        (list 'newline newline)
        (list 'display display)
        ;; 其他。。。
        ))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
    (primitive-implementation proc) args))

(define input-prompt ";;; L-Eval input:")

(define output-prompt ";;; L-Eval value:")

(define (drive-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    ;; debug
    ;; (announce-output (car input))
    ;; (announce-output (cdr input))
    (let ((output (actual-value input the-global-environment)))
      (announce-output output-prompt)
      (user-print input))) ;; changed
  (drive-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

;; ------------change-------------
;; 利用4.33进行函数初始化
(define lazy-pair-funcs
  '((define primitive-car car)

    (define primitive-cdr cdr)

    (define primitive-cons cons)

    (define (cons x y)
      (primitive-cons 'lazy-cons (lambda (m) (m x y))))

    (define (car z)
      ((primitive-cdr z) (lambda (p q) p)))

    (define (cdr z)
      ((primitive-cdr z) (lambda (p q) q)))

    (define (list-ref items n)
      (if (= n 0)
          (car items)
          (list-ref (cdr items) (- n 1))))

    (define (map proc items)
      (if (null? items)
          '()
          (cons (proc (car items))
                (map proc (cdr items)))))

    (define (scale-list items factor)
      (map (lambda (x) (* x factor))
           items))

    (define (add-lists list1 list2)
      (cond ((null? list1) list2)
            ((null? list2) list1)
            (else (cons (+ (car list1) (car list2))
                        (add-lists (cdr list1) (cdr list2))))))

    (define ones (cons 1 ones))

    (define integers (cons 1 (add-lists ones integers)))

    (define (lazy-pair? object)
      (if (pair? object)
          (eq? (primitive-car object) 'lazy-cons)
          false))

    (define (print object)
      (define (display-lazy-pair pair count)
        (cond ((null? pair) (display ""))
              ((= count 0)  (display "...)"))
              (else
                (display "(")
                (display (car pair))
                (if (lazy-pair? (cdr pair))
                    (begin (display " ")
                           (display-lazy-pair (cdr pair) (- count 1)))
                    (begin (display " . ")
                           (display (cdr pair))))
                (display ")"))))

      (if (lazy-pair? object)
          (display-lazy-pair object 10) ;; > 10认为是无穷表
          (display object)))
    ))

(define (init-lazy-pair-funcs env)
  (for-each (lambda (f) (eval f env))
            lazy-pair-funcs))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (meta-apply (lookup-variable-value 'print the-global-environment)
                  (list object)
                  the-global-environment)))
;; ------------change-------------

(define the-global-environment (setup-environment))

;; 将语法分析与执行分离
(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp)))
        ((application? exp) (analyze-application exp))
        (else
          (error "Unknown expression type -- ANALYZE" exp))))

(define (analyze-self-evaluating exp)
  (lambda (env) exp))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))

(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      'ok)))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env)
      (if (true? (pproc env))
          (cproc env)
          (aproc env)))))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env) (make-procedure vars bproc env))))

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application (fproc env)
                           (map (lambda (aproc) (aproc env))
                                aprocs)))))

(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))))
        (else
          (error
            "Unknown procedure type -- EXECUTE-APPLICATION"
            proc))))


(drive-loop)
