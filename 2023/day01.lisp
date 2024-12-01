;;;; day01.lisp

(in-package :aoc2023.day01)

(defun parse-digits (&rest digits)
  (parse-integer (format nil "~{~a~}" digits)))

(defun calibration-value (line)
  (parse-digits
   (find-if #'digit-char-p line)
   (find-if #'digit-char-p line :from-end t)))

(defun part1 (input)
  (reduce #'+ (mapcar #'calibration-value (aoc:lines input))))

(defparameter *digits*
  (loop for d from 1 upto 10
        collect (list (format nil "~r" d) d)
        collect (list (format nil "~d" d) d)))

(defun find-digits (line)
  (loop repeat (length line)
        for l = line then (str:substring 1 nil l)
        when (cadr (find-if (a:rcurry #'str:starts-with? l) *digits* :key #'car))
          collect it))

(defun calibration-value2 (line)
  (let ((digits (find-digits line)))
    (parse-digits (first digits) (a:lastcar digits))))

(defun part2 (input)
  (reduce #'+ (mapcar #'calibration-value2 (aoc:lines input))))
