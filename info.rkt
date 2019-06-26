#lang racket

(define (padding int)
  (if (< int 10)
      (string-append "0" (number->string int))
      (number->string int)))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-n e n)
  (if (= n 0)
      '()
      (cons e (enumerate-n e (- n 1)))))

(define count-list '(46 97 82 79 52))

(define (write-file file data)
  (with-output-to-file file
    (lambda ()
      (write data))
    #:mode 'binary
    #:exists 'replace))

(define (execise-file? s)
  (regexp-match "^ex" s))

(define (front lst n)
  (define (iter i lst)
    (if (or (= i n) (null? lst))
        '()
        (cons (car lst) (iter (+ i 1) (cdr lst)))))
  (iter 0 lst))

(define (behind lst n)
  (define (iter i lst)
    (if (or (= i n) (null? lst))
        lst
        (iter (+ i 1) (cdr lst))))
  (iter 0 lst))

(define (split lst n)
  (if (null? lst)
      '()
      (let ((nes (front lst n)))
        (cons nes (split (behind lst n) n)))))

(define (list-path-string parent)
  (map path->string (directory-list parent)))

(define (make-row rs)
  (string-append "|" (string-join rs "|") "|"))

(define (make-ex chapter-idx ex-idx)
  (string-append "ex" (number->string chapter-idx) "." (padding ex-idx) ".scm"))

(define (make-chapter chapter-idx)
  (string-append "chapter" (number->string chapter-idx)))

(define width 7)

(define readme-content
  (string-append
   "## 计算机程序的构造与解释(Structure and Interpretation of Computer Programs)

编程环境为DrRacket，版本7.1，[配置方法](https://docs.racket-lang.org/sicp-manual/)，没有选择mit-scheme，原因有二：
- 编辑和调试不好用
- 没有sicp图形语言"
   "\n\n习题进度\n\n"
   (string-join
   (map
    (lambda (chapter-idx)
      (let ((done (filter execise-file? (list-path-string (make-chapter chapter-idx))))
            (total-count (list-ref count-list (- chapter-idx 1))))
        (string-append
         (string-append "第"
                        (number->string chapter-idx)
                        "章：总量"
                        (number->string total-count)
                        "，完成"
                        (number->string (length done))
                        "\n\n")
         (string-join (append (list (make-row (map number->string (enumerate-interval 1 width)))
                                    (make-row (enumerate-n ":---:" width)))
                              (map make-row
                                   (split (map (lambda (ex-idx)
                                                 (let ((ex (make-ex chapter-idx ex-idx)))
                                                   (if (member ex done) ex "--")))
                                               (enumerate-interval 1 total-count))
                                          width)))
                      "\n"))))
    '(1 2 3 4))
   "\n\n\n")))

(call-with-output-file "README.md"
  (lambda(output-port)
    (display readme-content output-port))
  #:exists 'replace)
