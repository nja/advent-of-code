;;;; day18.lisp

(in-package #:aoc2018.day18)

(defun to-array (input &optional (dim 50))
  (loop with array = (make-array (list dim dim))
        with i = 0
        for c across input
        when (graphic-char-p c)
          do (setf (row-major-aref array i) c)
             (incf i)
        finally (return array)))

(defun count-to (array char n row col)
  (labels ((match (r c)
             (and (array-in-bounds-p array r c)
                  (equal char (aref array r c))))
           (row (r)
             (loop until (zerop n)
                   for c from (1- col) to (1+ col)
                   when (and (not (and (= row r) (= col c)))
                             (match r c))
                     do (decf n)
                   finally (return (zerop n)))))
    (or (row (1- row)) (row row) (row (1+ row)))))

(defun advance (array)
  (loop with next = (funcall #'make-array (array-dimensions array))
        for row below (array-dimension array 0)
        do (loop for col below (array-dimension array 1)
                 for n = (case (aref array row col)
                           (#\. (if (count-to array #\| 3 row col) #\| #\.))
                           (#\| (if (count-to array #\# 3 row col) #\# #\|))
                           (#\# (if (and (count-to array #\| 1 row col)
                                         (count-to array #\# 1 row col))
                                    #\# #\.)))
                 do (setf (aref next row col) n))
        finally (return next)))

(defun minutes (array n)
  (loop repeat n
        do (setf array (advance array))
        finally (return array)))

(defun resource-value (array)
  (loop for i below (array-total-size array)
        for c = (row-major-aref array i)
        count (equal c #\|) into trees
        count (equal c #\#) into lumberyards
        finally (return (* trees lumberyards) )))

(defun part1 (input)
  (resource-value (minutes (to-array input) 10)))

(defun cycle (array)
  (let ((seen (make-hash-table :test 'equalp)))
    (loop until (gethash array seen)
          do (setf (gethash array seen) (resource-value array))
             (setf array (advance array)))
    (loop with origin = (a:copy-array array)
          for i from 1
          do (setf array (advance array))
          until (equalp origin array)
          finally (return (values (hash-table-count seen) i origin)))))

(defun part2 (input)
  (multiple-value-bind (ramp-up cycle array) (cycle (to-array input))
    (resource-value (minutes array (rem (- 1000000000 ramp-up) cycle)))))
