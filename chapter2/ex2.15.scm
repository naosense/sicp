#lang sicp

(#%require "text.scm")
(#%require "ex2.12.scm")
(#%require "ex2.14.scm")

;; 如果变量重复出现就会将误差来回传递
;; 进而将误差变大
(percent (par1 A B))
(percent (par2 A B))
(center (par1 A B))
(center (par2 A B))