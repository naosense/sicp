#lang sicp

(#%require "lazy.scm")

(define (unless? exp) (tagged-list? exp 'unless))

(define (unless-predicate exp) (cadr exp))

(define (unless-usual exp) (caddr exp))

(define (unless-exception exp) (cadddr exp))

(define (unless->if exp)
  (make-if (unless-predicate exp)
           (unless-exception exp)
           (unless-usual exp)))

(display (unless->if '(unless (= b 0) (/ a b) b)))
