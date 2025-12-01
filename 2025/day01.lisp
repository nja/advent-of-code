;;;; day01.lisp

(in-package :aoc2025.day01)

(defun parse (line)
  (cons (case (aref line 0)
          (#\R 'R)
          (#\L 'L))
        (read-from-string line nil nil :start 1)))

(defun rotations (input)
  (mapcar #'parse (aoc:lines input)))

(defun count-zeroes (start rotations)
  (loop for (d . n) in rotations
        for dial = (mod (funcall (case d (R #'+) (L #'-)) (or dial start) n) 100)
        count (zerop dial)))

(defun part1 (input)
  (count-zeroes 50 (rotations input)))

(defun clicks (from direction n)
  (loop with dial = from
        repeat n
        do (case direction
             (R (incf dial))
             (L (decf dial)))
           (setf dial (mod dial 100))
        count (zerop dial) into clicks
        finally (return (values dial clicks))))

(defun count-zero-clicks (start rotations)
  (loop for (d . n) in rotations
        for (dial clicks) = (multiple-value-list (clicks (or dial start) d n))
        sum clicks))

(defun part2 (input)
  (count-zero-clicks 50 (rotations input)))
