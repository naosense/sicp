#lang sicp

;; node定义;;;;;;;;;;;;;;;;;;;;;;
(define (make-node val prev next)
  (cons val (cons prev next)))

(define (set-node-val! node val)
  (set-car! node val))

(define (get-node-val node)
  (car node))

(define (set-node-prev! node prev)
  (if (not (null? node))
      (set-car! (cdr node) prev)))

(define (get-node-prev node)
  (cadr node))

(define (set-node-next! node next)
  (if (not (null? node))
      (set-cdr! (cdr node) next)))

(define (get-node-next node)
  (cddr node))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (front-ptr deque) (car deque))

(define (rear-ptr deque) (cdr deque))

(define (set-front-ptr! deque item)
  (set-car! deque item))

(define (set-rear-ptr! deque item)
  (set-cdr! deque item))

(define (empty-deque? deque)
  (or (null? (front-ptr deque)) (null? (rear-ptr deque))))

(define (make-deque) (cons '() '()))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque)
      (get-node-val (front-ptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called with an empty deque" deque)
      (get-node-val (rear-ptr deque))))

(define (front-insert-deque! deque item)
  (let ((new-pair (make-node item '() (front-ptr deque))))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair)
           deque)
          (else
           (set-node-prev! (front-ptr deque) new-pair)
           (set-front-ptr! deque new-pair)
           deque))))

(define (rear-insert-deque! deque item)
  (let ((new-pair (make-node item (rear-ptr deque) '())))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair)
           deque)
          (else
           (set-node-next! (rear-ptr deque) new-pair)
           (set-rear-ptr! deque new-pair)
           deque))))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with an empty deque" deque))
        (else
         (let ((front-second (get-node-next (front-ptr deque))))
           (set-node-prev! front-second '())
           (set-node-next! (front-ptr deque) '())
           (set-front-ptr! deque front-second)
           (if (empty-deque? deque)
               (set-rear-ptr! deque (front-ptr deque)))
           deque))))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with an empty deque" deque))
        (else
         (let ((rear-second (get-node-prev (rear-ptr deque))))
           (set-node-next! rear-second '())
           (set-node-prev! (rear-ptr deque) '())
           (set-rear-ptr! deque rear-second)
           (if (empty-deque? deque)
               (set-front-ptr! deque (rear-ptr deque)))
           deque))))

(define (print-deque deque)
  (define (iter first)
    (cond ((null? first)
           (newline))
          (else
           (display (get-node-val first))
           (display " ")
           (iter (get-node-next first)))))
  (iter (front-ptr deque)))

(define q1 (make-deque))

(print-deque (front-insert-deque! q1 'a))
(print-deque (rear-insert-deque! q1 'b))
(print-deque (front-insert-deque! q1 'c))
(print-deque (front-insert-deque! q1 'd))
(print-deque (rear-insert-deque! q1 'e))
(print-deque (front-delete-deque! q1))
(print-deque (front-delete-deque! q1))
(print-deque (rear-delete-deque! q1))
(print-deque (front-delete-deque! q1))
(print-deque (rear-delete-deque! q1))
(print-deque (rear-delete-deque! q1))
