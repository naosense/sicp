;; #lang sicp-pict有bug导致课本中的有些程序无法运行
;; 这里参考网上资料，使用graphics/graphics中的基本函数搭建了一套
;; 图形语言
;;
;; 框架的向量示意图应该是这样的，frame origin pointer垂直于
;; edge1 vector和edge2 vector书本上的容易有误解
;
;;    ^
;;    | frame edge2 vector
;;    |
;;    |
;;    |
;;   _|__________>
;;    /
;;   /|         frame edge1 vector
;;  /
;; /
;;/ frame origin pointer
#lang racket

(require graphics/graphics)
(require "ex2.46.scm")
(require "ex2.47.scm")
(require "ex2.48.scm")

(open-graphics)
(define vp (open-viewport "A Picture Language" 500 500))
(define draw (draw-viewport vp))
(define (clear) ((clear-viewport vp)))
(define line (draw-line vp))


(define (paint painter)
  ;; 左上角为原点
  (painter (make-frame
            (make-vect 0 500)
            (make-vect 500 0)
            (make-vect 0 -500))))

(define (draw-line-v v1 v2)
  (define (vector-to-posn v)
    (make-posn (car v) (cdr v)))
  (line (vector-to-posn v1) (vector-to-posn v2)))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       ;; draw-line已经在graphics被占用了，所以改了下名字
       (draw-line-v
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))
  
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

(define wave
  (segments->painter
   (list
    (make-segment (make-vect 0.006 0.840) (make-vect 0.155 0.591))
    (make-segment (make-vect 0.006 0.635) (make-vect 0.155 0.392))
    (make-segment (make-vect 0.304 0.646) (make-vect 0.155 0.591))
    (make-segment (make-vect 0.298 0.591) (make-vect 0.155 0.392))
    (make-segment (make-vect 0.304 0.646) (make-vect 0.403 0.646))
    (make-segment (make-vect 0.298 0.591) (make-vect 0.354 0.492))
    (make-segment (make-vect 0.403 0.646) (make-vect 0.348 0.845))
    (make-segment (make-vect 0.354 0.492) (make-vect 0.249 0.000))
    (make-segment (make-vect 0.403 0.000) (make-vect 0.502 0.293))
    (make-segment (make-vect 0.502 0.293) (make-vect 0.602 0.000))
    (make-segment (make-vect 0.348 0.845) (make-vect 0.403 0.999))
    (make-segment (make-vect 0.602 0.999) (make-vect 0.652 0.845))
    (make-segment (make-vect 0.652 0.845) (make-vect 0.602 0.646))
    (make-segment (make-vect 0.602 0.646) (make-vect 0.751 0.646))
    (make-segment (make-vect 0.751 0.646) (make-vect 0.999 0.343))
    (make-segment (make-vect 0.751 0.000) (make-vect 0.597 0.442))
    (make-segment (make-vect 0.597 0.442) (make-vect 0.999 0.144)))))

(define wave-smile
  (segments->painter
   (list
    (make-segment (make-vect 0.45 0.75) (make-vect 0.5 0.7))
    (make-segment (make-vect 0.55 0.75) (make-vect 0.5 0.7))
    (make-segment (make-vect 0.006 0.840) (make-vect 0.155 0.591))
    (make-segment (make-vect 0.006 0.635) (make-vect 0.155 0.392))
    (make-segment (make-vect 0.304 0.646) (make-vect 0.155 0.591))
    (make-segment (make-vect 0.298 0.591) (make-vect 0.155 0.392))
    (make-segment (make-vect 0.304 0.646) (make-vect 0.403 0.646))
    (make-segment (make-vect 0.298 0.591) (make-vect 0.354 0.492))
    (make-segment (make-vect 0.403 0.646) (make-vect 0.348 0.845))
    (make-segment (make-vect 0.354 0.492) (make-vect 0.249 0.000))
    (make-segment (make-vect 0.403 0.000) (make-vect 0.502 0.293))
    (make-segment (make-vect 0.502 0.293) (make-vect 0.602 0.000))
    (make-segment (make-vect 0.348 0.845) (make-vect 0.403 0.999))
    (make-segment (make-vect 0.602 0.999) (make-vect 0.652 0.845))
    (make-segment (make-vect 0.652 0.845) (make-vect 0.602 0.646))
    (make-segment (make-vect 0.602 0.646) (make-vect 0.751 0.646))
    (make-segment (make-vect 0.751 0.646) (make-vect 0.999 0.343))
    (make-segment (make-vect 0.751 0.000) (make-vect 0.597 0.442))
    (make-segment (make-vect 0.597 0.442) (make-vect 0.999 0.144)))))

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0 1)
                     (make-vect 1 1)
                     (make-vect 0 0)))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1 0)
                     (make-vect 1 1)
                     (make-vect 0 0)))

;; ex2.50
(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1 1)
                     (make-vect 0 1)
                     (make-vect 1 0)))

(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0 1)
                     (make-vect 0 0)
                     (make-vect 1 1)))

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1 0)
                     (make-vect 0 0)
                     (make-vect 1 1)))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0 0)
                              split-point
                              (make-vect 0 1)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1 0)
                              (make-vect 0.5 1))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

;; ex2.51
(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-down
           (transform-painter painter1
                              (make-vect 0 0)
                              (make-vect 1 0)
                              split-point))
          (paint-up
           (transform-painter painter2
                              split-point
                              (make-vect 1 0.5)
                              (make-vect 0 1))))
      (lambda (frame)
        (paint-down frame)
        (paint-up frame)))))

;(paint wave)

;(paint (rotate270 wave))

;(paint (beside wave (flip-horiz wave)))
                     

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

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (shrink-to-upper-right painter)
  (transform-painter painter
                    (make-vect 0.5 0.5)
                    (make-vect 1.0 0.5)
                    (make-vect 0.5 1.0)))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(paint (square-limit wave-smile 4))




