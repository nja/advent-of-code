;;;; day01.lisp

(in-package :aoc2023.day01)

(defun calibration-value (line)
  (parse-integer (format nil "~d~d"
                         (find-if #'digit-char-p line)
                         (find-if #'digit-char-p line :from-end t))))

(defun part1 (input)
  (reduce #'+ (mapcar #'calibration-value (aoc:lines input))))