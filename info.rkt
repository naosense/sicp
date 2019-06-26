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

(define width 15)

(define (progress total done)
  (let ((len 60)
        (percent (/ done total)))
    (string-append "|"
                   (string-join (enumerate-n "=" (floor (* len percent))) "")
                   "|"
                   (number->string (floor (* percent 100)))
                   "% ("
                   (number->string done)
                   "/"
                   (number->string total)
                   ")")))

(define readme-content
  (string-append
   "## 计算机程序的构造与解释(Structure and Interpretation of Computer Programs)

### 环境搭建

编程环境为DrRacket，版本7.1，现在已经有了专门给本书编写的包[配置方法](https://docs.racket-lang.org/sicp-manual/)，没有选择mit-scheme，原因有二：

- 编辑和调试不好用
- 没有sicp图形语言

如果你喜欢使用Vim，那么下面的配置还可以一键运行，特别的方便。

```vim
augroup scheme
    autocmd!
    \" 加上<esc>可以避免弹出命令行必须按两次enter才能回到代码
    autocmd filetype scheme nnoremap <F9> :w<cr>:! racket %<cr><esc>
augroup end
```
这样直接按下`F9`就能运行了

### 遇到问题

#### Win10安装sicp包

7.1版本ui界面安装pkg报错cadr: contract violation，可以使用命令行安装，命令如下：

```shell
raco pkg install --auto sicp
```

#### 图形语言

sicp-pict包实现的图形语言有些习题不能做，可以使用`chapter2/pict.scm`代替。

#### 惰性求值

和mit-scheme不同，书本上的`delay`函数在racket中并不能延迟求值，可以用下面的代码实现：

```scheme
(define-syntax delay
  (syntax-rules ()
    ((delay expr)
     (memo-proc
      (lambda ()
        expr)))))
```

在流与延时求值那一节，课本上的solve函数不能运行，可以作如下修改：

```scheme
(define (solve f y0 dt)
  (define y (integral (delay (force dy)) y0 dt))
  (define dy (delay (stream-map f y)))
  y)
```
"
   "\n\n### 习题进度\n\n"
   (string-join
   (map
    (lambda (chapter-idx)
      (let ((done (filter execise-file? (list-path-string (make-chapter chapter-idx))))
            (total-count (list-ref count-list (- chapter-idx 1))))
        (string-append
         (string-append "#### 第"
                        (number->string chapter-idx)
                        "章："
                        (progress total-count (length done))
                        "\n\n")
         (string-join (append (list (make-row (map number->string (enumerate-interval 1 width)))
                                    (make-row (enumerate-n ":---:" width)))
                              (map make-row
                                   (split (map (lambda (ex-idx)
                                                 (let ((ex (make-ex chapter-idx ex-idx)))
                                                   (if (member ex done) (substring ex 2 6) "--")))
                                               (enumerate-interval 1 total-count))
                                          width)))
                      "\n"))))
    '(1 2 3 4))
   "\n\n\n")))

(call-with-output-file "README.md"
  (lambda(output-port)
    (display readme-content output-port))
  #:exists 'replace)
