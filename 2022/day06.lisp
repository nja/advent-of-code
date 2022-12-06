;;;; day06.lisp

(in-package #:aoc2022.day06)

(defun marker (length string)
  (loop for start from 0
        for end from length to (length string)
        unless (loop for i from start below end
                     for char = (aref string i)
                       thereis (< 1 (count char string :start start :end end)))
          do (return end)))

(defun part1 (input)
  (marker 4 input))

(defun part2 (input)
  (marker 14 input))
