;;;; day17.lisp

(in-package :aoc2015.day17)

(defun parse (input)
  (mapcar #'parse-integer (aoc:lines input)))

(defun combinations (liters containers &optional minimum)
  (loop with count = 0
        for i from 1 to (length containers)
        do (a:map-combinations
            (lambda (combination)
              (when (eql liters (reduce #'+ combination))
                (incf count)))
            containers
            :length i)
        until (and minimum (plusp count))
        finally (return count)))

(defun part1 (input)
  (combinations 150 (parse input)))

(defun part2 (input)
  (combinations 150 (parse input) t))
