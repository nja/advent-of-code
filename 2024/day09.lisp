;;;; day09.lisp

(in-package :aoc2024.day09)

(defun parse (input)
  (loop with fs = (make-array (* 10 (length input)) :fill-pointer 0)
        with id = 0
        for c across input
        for len = (digit-char-p c)
        for file? = t then (not file?)
        when (and file? len)
          collect (list id (fill-pointer fs) len) into files
        when len
          do (loop repeat len
                   do (vector-push (when file? id) fs))
        when file?
          do (incf id)
        finally (return (values fs files))))

(defun compact (array)
  (loop for i from 0
        while (< i (length array))
        when (null (aref array i))
          do (setf (aref array i)
                   (loop for x = (vector-pop array)
                         when x return it))
        finally (return array)))

(defun checksum (array)
  (loop for i from 0
        for id across array
        when id
          sum (* i id)))

(defun part1 (input)
  (checksum (compact (parse input))))

(defun defrag (array files)
  (loop with /dev/null = (make-array 10 :initial-element nil)
        for (id start len) in files
        for end = (+ start len)
        for skip = (position nil array :start (or skip 0))
        for space = (find-space array len skip start)
        when space
          do (replace array array :start1 space :start2 start :end2 end)
             (replace array /dev/null :start1 start :end1 end)
        finally (return array)))

(defun find-space (array size start end)
  (loop while (< start end)
        do (loop for i from start
                 for s from 1
                 while (null (aref array i))
                 when (= s size)
                   do (return-from find-space start)
                 finally (incf start s))))

(defun part2 (input)
  (multiple-value-bind (array files) (parse input)
    (checksum (defrag array (reverse files)))))
