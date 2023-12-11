;;;; day11.lisp

(in-package :aoc2023.day11)

(defparameter *test*
"...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....")

(defun expand (input)
  (loop with lines = (aoc:lines input)
        with empty-rows = (empty-rows input)
        with empty-cols = (empty-cols input)
        with array = (make-array (list (+ (length lines)
                                          (length empty-rows))
                                       (+ (length (first lines))
                                          (length empty-cols)))
                                 :initial-element #\.)
        for srow from 0
        for drow from 0
        for line in lines do
          (flet ((line ()
                   (loop for scol from 0
                         for dcol from 0
                         for x across line
                         do (setf (aref array drow dcol) x)
                         when (find scol empty-cols)
                           do (setf (aref array drow (incf dcol)) x))))
            (line)
            (when (find srow empty-rows)
              (incf drow)
              (line)))
        finally (return array)))

(defun empty-rows (input)
  (loop for l in (aoc:lines input)
        for r from 0
        when (every (a:curry #'char= #\.) l)
          collect r))

(defun empty-cols (input)
  (loop with lines = (aoc:lines input)
        for c below (length (first lines))
        when (every (lambda (l) (char= #\. (aref l c))) lines)
          collect c))

(defun galaxies (array)
  (loop for row below (array-dimension array 0)
        append (loop for col below (array-dimension array 1)
                     when (char= #\# (aref array row col))
                       collect (list row col))))

(defun distance (a b)
  (reduce #'+ (mapcar (lambda (x y) (abs (- x y))) a b)))

(defun distances (galaxies)
  (let ((sum 0))
    (a:map-combinations (lambda (x) (incf sum (apply #'distance x)))
                        galaxies
                        :length 2)
    sum))

(defun part1 (input)
  (distances (galaxies (expand input))))