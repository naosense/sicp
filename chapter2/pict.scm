;#lang sicp
#lang racket

(require sicp-pict)

;; 图形语言
;; (define ao (bitmap->painter "ao.png"))

(define letterlambda
  (segments->painter
   (list (make-segment (make-vect .45 .6) (make-vect .25 .2))
         (make-segment (make-vect .25 .2) (make-vect .2 .2))
         (make-segment (make-vect .2 .2) (make-vect .2 .1))
         (make-segment (make-vect .2 .1) (make-vect .3 .1))
         (make-segment (make-vect .3 .1) (make-vect .5 .5))
         (make-segment (make-vect .5 .5) (make-vect .7 .1))
         (make-segment (make-vect .7 .1) (make-vect .8 .1))
         (make-segment (make-vect .8 .1) (make-vect .8 .2))
         (make-segment (make-vect .8 .2) (make-vect .75 .2))
         (make-segment (make-vect .75 .2) (make-vect .4 .9))
         (make-segment (make-vect .4 .9) (make-vect .3 .9))
         (make-segment (make-vect .3 .9) (make-vect .3 .8))
         (make-segment (make-vect .3 .8) (make-vect .35 .8))
         (make-segment (make-vect .35 .8) (make-vect .45 .6)))))

(define wave-segments
  (list
   (make-segment (make-vect 0.006 0.840)
                 (make-vect 0.155 0.591))
   (make-segment (make-vect 0.006 0.635)
                 (make-vect 0.155 0.392))
   (make-segment (make-vect 0.304 0.646)
                 (make-vect 0.155 0.591))
   (make-segment (make-vect 0.298 0.591)
                 (make-vect 0.155 0.392))
   (make-segment (make-vect 0.304 0.646)
                 (make-vect 0.403 0.646))
   (make-segment (make-vect 0.298 0.591)
                 (make-vect 0.354 0.492))
   (make-segment (make-vect 0.403 0.646)
                 (make-vect 0.348 0.845))
   (make-segment (make-vect 0.354 0.492)
                 (make-vect 0.249 0.000))
   (make-segment (make-vect 0.403 0.000)
                 (make-vect 0.502 0.293))
   (make-segment (make-vect 0.502 0.293)
                 (make-vect 0.602 0.000))
   (make-segment (make-vect 0.348 0.845)
                 (make-vect 0.403 0.999))
   (make-segment (make-vect 0.602 0.999)
                 (make-vect 0.652 0.845))
   (make-segment (make-vect 0.652 0.845)
                 (make-vect 0.602 0.646))
   (make-segment (make-vect 0.602 0.646)
                 (make-vect 0.751 0.646))
   (make-segment (make-vect 0.751 0.646)
                 (make-vect 0.999 0.343))
   (make-segment (make-vect 0.751 0.000)
                 (make-vect 0.597 0.442))
   (make-segment (make-vect 0.597 0.442)
                 (make-vect 0.999 0.144))))

(define wave (segments->painter wave-segments))

(define (flip-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))
;(paint (right-split wave 4))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))
;(paint (up-split wave 4))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))
;(paint (corner-split wave 4))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))
(paint (square-limit wave 4))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

; 框架的向量示意图应该是这样的，frame origin pointer垂直于
; edge1 vector和edge2 vector书本上的容易有误解

;    ^
;    | frame edge2 vector
;    |
;    |
;    |
;   _|__________>
;    /
;   /|         frame edge1 vector
;  /
; /
;/ frame origin pointer

;(define (frame-coord-map frame)
;  (lambda (v)
;    (add-vect
;     (origin-frame frame)
;     (add-vect (scale-vect (xcor-vect v)
;                           (edge1-frame frame))
;               (scale-vect (ycor-vect v)
;                           (edge2-frame frame))))))
;
;(define (segments->painter segment-list)
;  (lambda (frame)
;    (for-each
;     (lambda (segment)
;       (draw-line
;        ((frame-coord-map frame) (start-segment segment))
;        ((frame-coord-map frame) (end-segment segment))))
;     segment-list)))
