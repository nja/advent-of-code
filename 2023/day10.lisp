;;;; day10.lisp

(in-package :aoc2023.day10)

(defparameter *test*
"..F7.
.FJ|.
SJ.L7
|F--J
LJ...")

(defun to-array (input)
  (loop with lines = (aoc:lines input)
        with array = (make-array (list (length lines) (length (first lines))))
        for c across (remove #\Newline input)
        for i from 0
        do (setf (row-major-aref array i) c)
        finally (return array)))

(defun turn (pipe dir)
  (ecase pipe
    (#\| (ecase dir (S 'S) (N 'N)))
    (#\- (ecase dir (W 'W) (E 'E)))
    (#\L (ecase dir (W 'N) (S 'E)))
    (#\J (ecase dir (S 'W) (E 'N)))
    (#\7 (ecase dir (E 'S) (N 'W)))
    (#\F (ecase dir (W 'S) (N 'E)))
    (#\S)))

(defun dir (direction)
  (getf '(N (-1  0) S ( 1  0) E ( 0  1) W ( 0 -1)) direction))

(defun add (a b)
  (mapcar #'+ a b))

(defun start-position (array)
  (loop for row below (array-dimension array 0) do
    (loop for col below (array-dimension array 1)
          when (eql #\S (aref array row col))
            do (return-from start-position (list row col)))))

(defun distance (array position direction)
  (loop for distance from 1
        for p = (add position (dir direction)) then (add p (dir dir))
        for pipe = (print (apply #'aref array p))
        for dir = (turn pipe (or dir direction))
        when (null dir)
          return distance))

(defun part1 (input)
  (let ((array (to-array input)))
    (/ (distance array (start-position array) 'S) 2)))
