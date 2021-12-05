(in-package #:cl-user)

(uiop:define-package #:day-4
  (:use #:coalton #:coalton-library))

(defun read-file-2 ()
  (with-open-file (stream (asdf:system-relative-pathname :advent-of-code-2021 "src/day-4.txt"))
    (loop :for line := (read-line stream nil)
          :while line
          :collect line)))

(in-package #:day-4)


(coalton-toplevel
  (declare numbers (List Integer))
  (define numbers (map (compose (fromSome "") parse-int) (split #\, "72,99,88,8,59,61,96,92,2,70,1,32,18,10,95,33,20,31,66,43,26,24,91,44,11,15,48,90,27,29,14,68,3,50,69,74,54,4,16,55,64,12,73,80,58,83,6,87,30,41,25,39,93,60,9,81,63,75,46,19,78,51,21,28,94,7,17,42,53,13,97,98,34,76,89,23,86,52,79,85,67,84,47,22,37,65,71,49,82,40,77,36,62,0,56,45,57,38,35,5")))


  (declare input (Vector String))
  (define input
    (into (lisp (List String) ()
            (cl-user::read-file-2))))

  (declare boards (Vector (Tuple (List (List (Tuple Integer (Cell Boolean)))) (Cell Boolean))))
  (define boards
    (progn
      (let boards = (make-vector))
      (let process-row =
        (fn (row)
          (map (compose (fn (x) (Tuple x (make-cell False))) (compose (fromSome "") parse-int)) (filter (/= "") (split #\SPACE row)))))
      (vector-chunked
       (fn (s)
         (vector-push
          (Tuple
           (make-list
            (process-row (slice-index-unsafe 0 s))
            (process-row (slice-index-unsafe 1 s))
            (process-row (slice-index-unsafe 2 s))
            (process-row (slice-index-unsafe 3 s))
            (process-row (slice-index-unsafe 4 s)))
           (make-cell False))
          boards))
       6 input)
      boards))

  (declare row-is-filled ((List (Tuple Integer (Cell Boolean))) -> Boolean))
  (define (row-is-filled row)
    (progn 
      (if (all (compose cell-read snd) row)
          (progn
            True)
          False)))

  (declare board-is-won ((List (List (Tuple Integer (Cell Boolean)))) -> Boolean))
  (define (board-is-won board)
    (or (any row-is-filled board)
        (any row-is-filled (transpose board))))

  (declare apply-number (Integer -> (Tuple Integer (Cell Boolean)) -> Unit))
  (define (apply-number n cell)
    (progn
      (when (== n (fst cell))
        (cell-write True (snd cell)))))

  (declare apply-number-to-board (Integer -> (List (List (Tuple Integer (Cell Boolean)))) -> Unit))
  (define (apply-number-to-board n board)
    (progn
      (map (map (apply-number n)) board)
      Unit))

  (declare score-board ((List (List (Tuple Integer (Cell Boolean)))) -> Integer))
  (define (score-board board)
    (sum (map (fold
               (fn (square total)
                 (if (not (cell-read (snd square)))
                     (+ total (fst square))
                     total))
               0)
              board)))

  (declare run ((List Integer) -> Unit))
  (define (run input)
    (match input
      ((Cons x xs)
       (progn 
         (let done = (make-cell False))
         (vector-foreach
          (fn (b)
            (progn
              (let board = (fst b))
              (apply-number-to-board x board)
              (when (board-is-won board)
                (traceObject "score" (* x (score-board board)))
                (cell-write True done))))
          boards)
         (unless (cell-read done)
           (run xs))
         Unit))
      ((Nil) Unit)))

  (declare run2 (Unit -> Unit))
  (define (run2 _)
    (let ((total (vector-length boards))
          (total-won (make-cell 0))
          (inner
            (fn (input)
              (match input
                ((Cons x xs)
                 (progn
                   (vector-foreach
                    (fn (b)
                      (progn
                        (let board = (fst b))
                        (let won = (snd b))
                        (unless (cell-read won)
                          (apply-number-to-board x board)
                          (when (board-is-won board)
                            (cell-update (+ 1) total-won)
                            (cell-write True won))
                          (when (== total (cell-read total-won))
                            (traceObject "score" (* x (score-board board)))))))
                    boards)
                   (unless (== total (cell-read total-won))
                     (inner xs))))
                ((Nil) Unit)))))
      (inner numbers))))
