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
        do (tilt* array 0 1 col 0)))

(defun tilt-south (array)
  (loop for col below (array-dimension array 1)
        do (tilt* array (1- (array-dimension array 0)) -1 col 0)))

(defun tilt-east (array)
  (loop for row below (array-dimension array 0)
        do (tilt* array row 0 (1- (array-dimension array 1)) -1)))

(defun tilt-west (array)
  (loop for row below (array-dimension array 0)
        do (tilt* array row 0 0 1)))

(defun cycles (array n)
  (dotimes (x n array)
    (tilt-north array)
    (tilt-west array)
    (tilt-south array)
    (tilt-east array)))

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

(defun tilt* (array start-row rd start-col cd)
  (flet ((free? (c) (char= #\. c))
         (round? (c) (char= #\O c))
         (square? (c) (char= #\# c)))    
    (loop with fr and fc
          for row = start-row then (+ row rd)
          for col = start-col then (+ col cd)
          while (array-in-bounds-p array row col)
          for c = (aref array row col)
          do (cond ((and fr (round? c))
                    (rotatef (aref array row col) (aref array fr fc))
                    (incf fr rd)
                    (incf fc cd))
                   ((or (round? c) (square? c))
                    (setf fr nil
                          fc nil))
                   ((and (not fr) (free? c))
                    (setf fr row
                          fc col))))))

(defun total-load (array)
  (let ((rows (array-dimension array 0)))
    (loop for row below rows
          sum (loop for col below (array-dimension array 1)
                    when (char= #\O (aref array row col))
                      sum (- rows row)))))

(defun part1 (input)
  (total-load (tilt-north (to-array input))))