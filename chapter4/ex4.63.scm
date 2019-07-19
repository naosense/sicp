#lang sicp

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

;;; Query input:
(grandson Cain ?s)

;;; Query results:
(grandson Cain Irad)

;;; Query input:
(parent Lamech ?s)

;;; Query results:
(parent Lamech Jubal)
(parent Lamech Jabal)

;;; Query input:
(grandson Methushael ?s)

;;; Query results:
(grandson Methushael Jubal)
(grandson Methushael Jabal)
