;;;; day18.lisp

(in-package #:aoc2016.day18)

(defun parse (input)
  (loop with array = (make-array (length input) :fill-pointer 0)
        for c across input
        do (case c
             (#\. (vector-push t array))
             (#\^ (vector-push nil array)))
        finally (return array)))

(defun trap? (left-safe center-safe right-safe)
  (let ((left-trap (not left-safe))
        (center-trap (not center-safe))
        (right-trap (not right-safe)))
    (or (and left-trap center-trap right-safe)
        (and left-safe center-trap right-trap)
        (and left-trap center-safe right-safe)
        (and left-safe center-safe right-trap))))

(defun next-row (row)
  (flet ((ref (i)
           (or (not (array-in-bounds-p row i))
               (aref row i))))
    (loop with next = (make-array (length row) :fill-pointer 0)
          for i below (length row)
          for left = t then center
          for center = (ref i) then right
          for right = (ref (1+ i))
          do (vector-push (not (trap? left center right)) next)
          finally (return next))))

(defun count-safe (row n)
  (loop for r = row then (next-row r)
        sum (loop for x across r count x)
        repeat n))

(defun part1 (input)
  (count-safe (parse input) 40))

(defun part2 (input)
  (count-safe (parse input) 400000))
