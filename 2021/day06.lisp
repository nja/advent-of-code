;;;; day06.lisp

(in-package #:aoc2021.day06)

(defun parse (input)
  (let ((contents (mapcar #'parse-integer (ppcre:split "," input))))
    (make-array (length contents) :initial-contents contents
                                  :adjustable t)))

(defun simulate (fish)
  (loop for i below (length fish)
        when (< (decf (aref fish i)) 0)
          do (setf (aref fish i) 6)
             (vector-push-extend 8 fish)
        finally (return fish)))

(defun days (n fish)
  (loop repeat n do (simulate fish)
        finally (return fish)))

(defun part1 (input)
  (length (days 80 (parse input))))

(defun counts (fish)
  (loop with array = (make-array 9 :initial-element 0)
        for i below (length fish)
        do (incf (aref array (elt fish i)))
        finally (return array)))

(defun shift (array)
  (prog1 (aref array 0)
    (loop for i from 0
          for j from 1 below (length array)
          do (setf (aref array i) (aref array j))
          finally (setf (aref array i) 0))))

(defun simulate-counts (array)
  (let ((spawn (shift array)))
    (setf (aref array 8) spawn)
    (incf (aref array 6) spawn)))

(defun days-counts (n array)
  (loop repeat n do (simulate-counts array)
        finally (return array)))

(defun part2 (input)
  (reduce #'+ (days-counts 256 (counts (parse input)))))
