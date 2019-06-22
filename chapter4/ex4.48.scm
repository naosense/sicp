(define adjs '(adj good tall cute))

(define (parse-simple-noun-phrase)
  (amb (list 'simple-noun-phrase
             (parse-word articles)
             (parse-word nouns))

       (list 'simple-noun-phrase
             (parse-word articles)
             (parse-word adjs)
             (parse-word nouns))))

(parse '(the good student with the cute cat sleeps in the class))
