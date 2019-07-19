(assert! (son Adam Cain))
(assert! (son Cain Enoch))
(assert! (son Enoch Irad))
(assert! (son Irad Mehujael))
(assert! (son Mehujael Methushael))
(assert! (son Methushael Lamech))
(assert! (wife Lamech Ada))
(assert! (son Ada Jabal))
(assert! (son Ada Jubal))

(assert! (rule (grandson ?g ?s)
               (and (parent ?g ?f)
                    (parent ?f ?s))))

(assert! (rule (parent ?p ?s)
               (or (son ?p ?s)
                   (and (wife ?p ?w)
                        (son ?w ?s))
                   (and (wife ?m ?p)
                        (son ?m ?s)))))

(assert! (rule (end-in-grandson (grandson))))
(assert! (rule (end-in-grandson (?x . ?rest))
               (end-in-grandson ?rest)))

(assert! (rule ((grandson) ?x ?y)
               (grandson ?x ?y)))

(assert! (rule ((great . ?rel) ?x ?y)
               (and (end-in-grandson ?rel)
                    (parent ?x ?z)
                    (?rel ?z ?y))))


;;; Query input:

((great grandson) ?g ?ggs)

;;; Query results:
((great grandson) Mehujael Jubal)
((great grandson) Irad Lamech)
((great grandson) Mehujael Jabal)
((great grandson) Enoch Methushael)
((great grandson) Cain Mehujael)
((great grandson) Adam Irad)

;;; Query input:
(?relationship Adam Irad) ;; 卡住了
