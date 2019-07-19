假如将wheel修改一下，
(rule (wheel2 ?person ?middle-manager ?x)
          (and (supervisor ?middle-manager ?person)
               (supervisor ?x ?middle-manager)))
就可以看到原因了：
;;; Query input:
(wheel2 ?x ?y ?z)

;;; Query results:
(wheel2 (Warbucks Oliver) (Scrooge Eben) (Cratchet Robert))
(wheel2 (Bitdiddle Ben) (Hacker Alyssa P) (Reasoner Louis))
(wheel2 (Warbucks Oliver) (Bitdiddle Ben) (Tweakit Lem E))
(wheel2 (Warbucks Oliver) (Bitdiddle Ben) (Fect Cy D))
(wheel2 (Warbucks Oliver) (Bitdiddle Ben) (Hacker Alyssa P))
