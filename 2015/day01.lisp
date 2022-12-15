;;;; day01.lisp

(in-package #:aoc2015.day01)

(defun floors (input &key target)
  (loop for c across input
        for i from 1
        sum (ecase c (#\( 1) (#\) -1)) into floor
        when (eq target floor)
          do (return i)
        finally (return floor)))

(defun part1 (input)
  (floors input))

(defun part2 (input)
  (floors input :target -1))
