;;;; day18.lisp

(in-package :aoc2015.day18)

(defun to-array (input)
  (loop with dim = (length (aoc:lines input))
        with array = (make-array (list dim dim))
        for row from 0
        for line in (aoc:lines input)
        do (loop for col from 0
                 for c across line
                 do (setf (aref array row col) c))
        finally (return array)))

(defparameter *corners* nil)

(defun neighbours? (array row col max)
  (flet ((ref (row col)
           (and (array-in-bounds-p array row col)
                (aref array row col))))
    (loop for (r c) in '((-1 -1) (-1 0) (-1 1)
                         ( 0 -1)        ( 0 1)
                         ( 1 -1) ( 1 0) ( 1 1))
          count (eql #\# (ref (+ row r) (+ col c)))
            into neighbours
          while (<= neighbours max)
          finally (return neighbours))))

(defun next (array row col)
  (case (aref array row col)
    (#\#
     (case (neighbours? array row col 3)
       ((2 3) #\#)
       (t #\.)))
    (#\.
     (case (neighbours? array row col 3)
       (3 #\#)
       (t #\.)))))

(defun steps (array n)
  (loop with next = (make-array (array-dimensions array))
        repeat n
        do (loop for row below (array-dimension array 0)
                 do (loop for col below (array-dimension array 1)
                          do (setf (aref next row col)
                                   (next array row col))))
           (when *corners*
             (corners next))
           (rotatef array next)
        finally (return array)))

(defun count-lights (array)
  (loop for i below (array-total-size array)
        count (eql #\# (row-major-aref array i))))

(defun part1 (input)
  (count-lights (steps (to-array input) 100)))

(defun corners (array)
  (setf (aref array 0 0) #\#
        (aref array 0 (1- (array-dimension array 1))) #\#
        (aref array (1- (array-dimension array 0)) 0) #\#
        (aref array
              (1- (array-dimension array 0))
              (1- (array-dimension array 1))) #\#))

(defun part2 (input)
  (let ((*corners* t))
    (count-lights (steps (to-array input) 100))))
