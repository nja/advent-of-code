;;;; day09.lisp

(in-package #:aoc2022.day09)

(defun parse (input)
  (mapcar #'aoc:read-as-list (aoc:lines input)))

(defun pos (x y) (list x y))
(defun add (a b) (mapcar #'+ a b))
(defun sub (a b) (mapcar #'- a b))

(defun drag (head tail)
  (let ((d (sub head tail)))
    (if (< 1 (apply #'max (mapcar #'abs d)))
        (mapcar (a:rcurry #'a:clamp -1 1) d)
        (pos 0 0))))

(defun rope (length)
  (loop repeat length collect (pos 0 0)))

(defparameter *track* nil)

(defun track (pos)
  (incf (gethash pos *track* 0))
  pos)

(defun move (pos dir)
  (add pos (case dir
             (u (pos 0 -1)) (l (pos -1 0))
             (d (pos 0  1)) (r (pos  1 0)))))

(defun pull (tail head)
  (add tail (drag head tail)))

(defun tug (head rope)
  (if (null rope)
      (cons (track head) nil)
      (cons head (tug (pull (first rope) head) (rest rope)))))

(defun simulate (rope moves)
  (reduce (lambda (state move)
            (loop with (dir n) = move
                  for (head . rope) = state
                  repeat n
                  do (setf state (tug (move head dir) rope))
                  finally (return state)))
          moves :initial-value rope))

(defun part1 (input)
  (let ((*track* (make-hash-table :test 'equalp)))
    (simulate (rope 2) (parse input))
    (hash-table-count *track*)))

(defun part2 (input)
  (let ((*track* (make-hash-table :test 'equalp)))
    (simulate (rope 10) (parse input))
    (hash-table-count *track*)))
