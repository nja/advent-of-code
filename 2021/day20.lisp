;;;; day20.lisp

(in-package #:aoc2021.day20)

(defparameter *algorithm* nil)

(defun parse-algorithm (input)
  (parse-integer (reverse (aoc:tr ".#" "01" (first (aoc:sections input)))) :radix 2))

(defun value (input)
  (ldb (byte 1 input) *algorithm*))

(defun advance (input x y z)
  (let ((shifted (logand (ldb (byte 9 0) -1) (ash input 1))))
    (flet ((setp (position val)
             (setf shifted (dpb val (byte 1 position) shifted))))
      (setp 6 x)
      (setp 3 y)
      (setp 0 z))
    shifted))

(defun parse-image (input)
  (let* ((lines (aoc:lines (second (aoc:sections input))))
         (size (length (first lines)))
         (array (make-array (list (+ 2 size )
                                  (+ 2 size))
                            :initial-element 0)))
    (loop for row from 1
          for line in lines
          do (loop for col from 1
                   for ch across line
                   do (setf (aref array row col) (case ch (#\. 0) (#\# 1)))))
    array))

(defun enhance (image)
  (let ((result (grow image))
        (background (aref image 0 0)))
    (flet ((read* (row col)
             (let ((row (1- row))
                   (col (1- col)))
              (if (array-in-bounds-p image row col)
                  (aref image row col)
                  background))))
      (loop for row from 0 below (array-dimension result 0) do
        (loop initially (setf input (if (zerop background) 0 -1))
              for col from 0 below (array-dimension result 1)
              for x = (read* (1- row) (1+ col))
              for y = (read*     row  (1+ col))
              for z = (read* (1+ row) (1+ col))
              for input = (advance input x y z)
              do (setf (aref result row col) (value input)))))
    result))

(defun grow (image)
  (make-array (list (+ 2 (array-dimension image 0))
                    (+ 2 (array-dimension image 1)))
              :initial-element 0))

(defun count-lit (image)
  (loop for i below (array-total-size image)
        count (eql 1 (row-major-aref image i))))

(defun part1 (input)
  (let ((*algorithm* (parse-algorithm input)))
    (count-lit (enhance (enhance (parse-image input))))))

(defun part2 (input)
  (let ((*algorithm* (parse-algorithm input))
        (image (parse-image input)))
    (loop repeat 50 do (setf image (enhance image)))
    (count-lit image)))
