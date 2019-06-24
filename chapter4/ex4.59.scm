;; a)
(meeting ?department (Friday ?time))

;; b)
(rule (meeting-time ?person ?day-and-time)
      (or (meeting whole-company ?day-and-time)
          (and (job ?person (?department . ?postion))
               (meeting ?department ?day-and-time))))

;; c)
(meeting-time (Hacker Alyssa P) (Wednesday ?time))
