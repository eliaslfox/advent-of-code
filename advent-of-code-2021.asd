(asdf:defsystem #:advent-of-code-2021
  :depends-on (#:coalton #:split-sequence)
  :pathname "src/"
  :serial t
  :components ((:file "day-1")
               (:file "day-2")
               (:file "day-4")))
