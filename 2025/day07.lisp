;;;; day07.lisp

(in-package :aoc2025.day07)

(defun parse (input)
  (aoc:to-array (aoc:tr "S" "|" input)))

(defparameter *test*
".......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............")

(defun beam-row (array row)
  (loop for col below (array-dimension array 1)
        for x = (aref array row col)
        for y = (aref array (1+ row) col)
        for beam? = (char= x #\|)
        for split? = (char= y #\^)
        count (and beam? split? )
        when (and beam? (not split?))
          do (setf (aref array (1+ row) col) #\|)
        when (and beam? split?)
          do (setf (aref array (1+ row) (1- col)) #\|
                   (aref array (1+ row) (1+ col)) #\|)))

(defun beam (array)
  (loop for row from 0 below (1- (array-dimension array 0))
        sum (beam-row array row)))

(defun part1 (input)
  (beam (parse input)))

;; 2025 07 1: '1507'
;; That's the right answer!
;; (Cached until 2025-12-07 05:18:54)
;; 1507

(defun quantum-row (array row timelines)
  (loop for col rom 0))