(uiop:define-package #:day-2
  (:use #:coalton #:coalton-library))

(defun read-input ()
  (with-open-file (stream (asdf:system-relative-pathname :advent-of-code-2021 "src/day-2.txt"))
    (coalton-library:cl-list-to-coalton
     (loop :for line := (read-line stream nil)
           :while line
           :for split := (split-sequence:split-sequence #\SPACE line)
           :collect (coalton-library:Tuple (first split) (parse-integer (second split)))))))

(cl:in-package #:day-2)

(coalton-toplevel
  (declare input (List (Tuple String Integer)))
  (define input (lisp (List (Tuple String Integer)) ()
                  (cl-user::read-input)))

  (declare process-step ((Tuple String Integer) -> (Tuple Integer Integer) -> (Tuple Integer Integer)))
  (define (process-step action state)
    (progn
      (let horizontal = (fst state))
      (let vertical = (snd state))

      (let direction = (fst action))
      (let magnitude = (snd action))

      (match direction
        ("forward" (Tuple (+ horizontal magnitude) vertical))
        ("down" (Tuple horizontal (+ vertical magnitude)))
        ("up" (Tuple horizontal (- vertical magnitude))))))


  (define output (fold process-step (Tuple 0 0) input))

  (declare process-step2 ((Tuple String Integer) -> (Tuple3 Integer Integer Integer) -> (Tuple3 Integer Integer Integer)))
  (define (process-step2 action state)
    (match state
      ((Tuple3 horizontal vertical aim)
       (progn
         (let direction = (fst action))
         (let magnitude = (snd action))

         (match direction
           ("forward" (Tuple3 (+ horizontal magnitude) (+ vertical (* magnitude aim)) aim))
           ("down" (Tuple3 horizontal vertical (+ aim magnitude)))
           ("up" (Tuple3 horizontal vertical (- aim magnitude))))))))

  (define output2 (fold process-step2 (Tuple3 0 0 0) input)))
