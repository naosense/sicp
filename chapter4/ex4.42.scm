;; ((betty 3) (ethel 5) (joan 2) (kitty 1) (mary 4))
;; 原本想使用or，但是注意审题，是必有一句真一句假，or的话
;; 两句都为真也可以

(define (xor a b)
  (if a
      (not b)
      b))

(define (and a b)
  (if a
      b
      false))

(define (or a b)
  (if a
      true
      b))

(define (lier-problem)
  (let ((betty (amb 1 2 3 4 5))
        (ethel (amb 1 2 3 4 5))
        (joan (amb 1 2 3 4 5))
        (kitty (amb 1 2 3 4 5))
        (mary (amb 1 2 3 4 5)))
    (require (xor (= kitty 2) (= betty 3)))
    (require (xor (= ethel 1) (= joan 2)))
    (require (xor (= joan 3) (= ethel 5)))
    (require (xor (= kitty 2) (= mary 4)))
    (require (xor (= mary 4) (= betty 1)))
    (require (distinct? (list betty ethel joan kitty mary)))
    (list (list 'betty betty)
          (list 'ethel ethel)
          (list 'joan joan)
          (list 'kitty kitty)
          (list 'mary mary))))
