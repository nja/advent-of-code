;;;; day14.lisp

(in-package :aoc2023.day14)

(defparameter *test*
"O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....")

(defun to-array (input)
  (loop with array = (make-array (list (length (aoc:lines input))
                                       (length (first (aoc:lines input)))))
        for i from 0
        for c across (remove #\Newline input)
        do (setf (row-major-aref array i) c)
        finally (return array)))

(defun tilt-north (array)
  (loop for col below (array-dimension array 1)
        do (tilt-north-col array col)
        finally (return array)))

(defun tilt-north-col (array col)
  (flet ((free? (c) (char= #\. c))
         (round? (c) (char= #\O c))
         (square? (c) (char= #\# c)))    
    (loop with free
          for row below (array-dimension array 0)
          for c = (aref array row col)
          do (cond ((and free (round? c))
                    (rotatef (aref array row col) (aref array free col))
                    (incf free))
                   ((or (round? c) (square? c))
                    (setf free nil))
                   ((and (not free) (free? c))
                    (setf free row))))))

(defun total-load (array)
  (let ((rows (array-dimension array 0)))
    (loop for row below rows
          sum (loop for col below (array-dimension array 1)
                    when (char= #\O (aref array row col))
                      sum (- rows row)))))

(defun part1 (input)
  (total-load (tilt-north (to-array input))))