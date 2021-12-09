;;;; day09.lisp

(in-package #:aoc2021.day09)

(defun to-array (input)
  (let* ((lines (aoc:lines input))
         (rows (length lines))
         (cols (length (first lines))))
    (loop with array = (make-array (list rows cols))
          with i = 0
          for c across input
          for n = (digit-char-p c)
          when n do (setf (row-major-aref array i) n)
            (incf i)
          finally (return array))))

(defun every-neighbour (array p row col)
  (loop for (x y) in '((-1 0) (0 -1) (1 0) (0 1))
        for r = (+ row x)
        for c = (+ col y)
        always (or (not (array-in-bounds-p array r c))
                   (funcall p (aref array r c)))))

(defun part1 (input)
  (loop with array = (to-array input)
        for row below (array-dimension array 0)
        sum (loop for col below (array-dimension array 1)
                  for x = (aref array row col)
                  when (every-neighbour array (a:curry #'< x) row col)
                    sum (1+ x))))

(defun mark-basins (array)
  (loop with basin = 0
        for row below (array-dimension array 0)
        nconc (loop for col below (array-dimension array 1)
                    for v = (aref array row col)
                    for b = (mark-basin array (list basin) row col)
                    when (< 0 b) collect b
                    and do (incf basin))))

(defun mark-basin (array basin row col)
  (cond ((begin-basin? (aref array row col))
         (setf (aref array row col) basin)
         (1+ (loop for (x y) in '((-1 0) (0 -1) (1 0) (0 1))
                   for r = (+ row x)
                   for c = (+ col y)
                   when (array-in-bounds-p array r c)
                     sum (mark-basin array basin r c))))
        (t 0)))

(defun begin-basin? (x)
  (and (integerp x) (< x 9)))

(defun part2 (input)
  (reduce #'* (subseq (sort (mark-basins (to-array input)) #'>) 0 3)))
