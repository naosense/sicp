#lang sicp

(#%require sicp-pict)

;; 图形语言
;; (define ao (bitmap->painter "ao.png"))
(define wave einstein)
(define wave2 (beside wave (flip-vert wave)))
(define wave4 (below wave2 wave2))
;(paint wave)
;;(paint wave4)

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
;(paint (square-limit wave 4))

