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
        for pipe = (apply #'aref array p)
        for dir = (turn pipe (or dir direction))
        when (null dir)
          return distance))

(defun part1 (input)
  (let ((array (to-array input)))
    (/ (distance array (start-position array) 'S) 2)))

(defun mark-big-loop (array position direction)
  (loop with copy = (make-array (mul (add (array-dimensions array) '(2 2)) 3) :initial-element #\.)
        for p = (add position (dir direction)) then (add p (dir dir))
        for pipe = (apply #'aref array p)
        for dir = (turn pipe (or dir direction))
        do (write-big-pipe copy p pipe)
        when (null dir)
          return copy))

(defun mul (a f)
  (mapcar (a:curry #'* f) a))

(defun write-big-pipe (big-array pos pipe)
  (let ((pos (mul (add pos '(1 1)) 3))
        (src (getf *big-pipes* pipe)))
    (loop for big-row from (first pos)
          for src-row below 3
          do (loop for big-col from (second pos)
                   for src-col below 3
                   do (setf (aref big-array big-row big-col)
                            (aref src src-row src-col))))))

(defparameter *big-pipes*
  (list #\| (to-array
"_|_
_|_
_|_")
        #\- (to-array
"___
---
___")
        #\L (to-array
"_L_
_LL
___")
        #\J (to-array
"_J_
JJ_
___")
        #\7 (to-array
"___
77_
_7_")
        #\F (to-array
"___
_FF
_F_")
        #\S (to-array
"_S_
_S_
_S_")))



(defun part2 (input)
  (let ((array (to-array input)))
    (mark-big-loop array (start-position array) 'S)))