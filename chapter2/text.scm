#lang sicp

(#%require "../chapter1/text.scm")
(#%require "ex2.56.scm")

(#%provide (all-defined))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-third (make-rat 1 3))

;; (print-rat one-third)

(define (make-interval  a b)
  (cons a b))

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

(define (add-interval x y)
  (make-interval (+ (lower-bound x)
                    (lower-bound y))
                 (+ (upper-bound x)
                    (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1 (upper-bound y))
                               (/ 1 (lower-bound y)))))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

;; 流处理
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

;;(display (accumulate + 0 (car '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))))

;;(flatmap (lambda (x) (list (square x))) '(2 3))

(define (permutations s)
  (if (null? s)
      (list nil)
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

;;(display (permutations '(1 2 3)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

;;(display (prime-sum-pairs 3))

;(display (append (list 1 2) '(())))
;(display (append (list (list 1 2)) '(())))
;(display (cons (list 1 2) '(())))

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

;; 求导函数
(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (addend s) (cadr s))

;(define (augend s) (caddr s))
; ex2.57
(define (augend s)
  (if (= (length s) 3)
      (caddr s)
      (cons '+ (cddr s))))

(define (multiplier p) (cadr p))

;(define (multiplicand p) (caddr p))
; ex2.57
(define (multiplicand p)
  (if (= (length p) 3)
      (caddr p)
      (cons '* (cddr p))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (let ((n (exponent exp))
               (u (base exp)))
           (make-product n
                         (make-product (deriv u var)
                                       (make-exponentiation u (- n 1))))))
        (else
         (error "unknown expression type -- DERIV" exp))))

;(display (deriv '(+ x 3) 'x))
;(display (deriv '(** x 3) 'x))
;(display (deriv '(* (* x y) (+ x 3)) 'x))
;(display (deriv '(* x y (+ x 3)) 'x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 集合作为未排序的表
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

;;(define (adjoin-set x set)
;;  (if (element-of-set? x set)
;;      set
;;      (cons x set)))
;;
;;(define (intersection-set set1 set2)
;;  (cond ((or (null? set1) (null? set2)) '())
;;        ((element-of-set? (car set1) set2)
;;         (cons (car set1) (intersection-set (cdr set1) set2)))
;;        (else (intersection-set (cdr set1) set2))))

;(display (intersection-set '(2 1 2 3 3) '(2 3 4 5)))

;; 集合作为排序的表
;;(define (element-of-set? x set)
;;  (cond ((null? set) false)
;;        ((= x (car set)) true)
;;        ((< x (car set)) false)
;;        (else (element-of-set? x (cdr set)))))
;;
;;(define (intersection-set set1 set2)
;;  (if (or (null? set1) (null? set2))
;;      '()
;;      (let ((x1 (car set1)) (x2 (car set2)))
;;        (cond ((= x1 x2)
;;               (cons x1
;;                     (intersection-set (cdr set1)
;;                                       (cdr set2))))
;;              ((< x1 x2)
;;               (intersection-set (cdr set1) set2))
;;              ((> x1 x2)
;;               (intersection-set set1 (cdr set2)))))))

;; 集合作为二叉树
;;(define (entry tree) (car tree))
;;
;;(define (left-branch tree) (cadr tree))
;;
;;(define (right-branch tree) (caddr tree))
;;
;;(define (make-tree entry left right)
;;  (list entry left right))
;;
;;(define (element-of-set? x set)
;;  (cond ((null? set) false)
;;        ((= x (entry set)) true)
;;        ((< x (entry set))
;;         (element-of-set? x (left-branch set)))
;;        ((> x (entry set))
;;         (element-of-set? x (right-branch set)))))
;;
;;(define (adjoin-set x set)
;;  (cond ((null? set) (make-tree x '() '()))
;;        ((= x (entry set)) set)
;;        ((< x (entry set))
;;         (make-tree (entry set)
;;                    (adjoin-set x (left-branch set))
;;                    (right-branch set)))
;;        ((> x (entry set))
;;         (make-tree (entry set)
;;                    (left-branch set)
;;                    (adjoin-set x (right-branch set))))))


;; 集合和信息化检索
;;(define (lookup given-key set-of-records)
;;  (cond ((null? set-of-records) false)
;;        ((equal? given-key (key (car set-of-records)))
;;         (car set-of-records))
;;        (else (lookup given-key (cdr set-of-records)))))
;; 示例Huffman树
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-l bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-l (cdr bits) tree))
              (decode-l (cdr bits) next-branch)))))
  (decode-l bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))

;;(display (make-leaf-set '((A 4) (B 2) (C 1) (D 1))))
                               
              