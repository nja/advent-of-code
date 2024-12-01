;;;; day01.lisp

(in-package :aoc2019.day01)

(defun parse (input)
  (mapcar #'parse-integer (aoc:lines input)))

(defun fuel (mass)
  (- (truncate mass 3) 2))

(defun part1 (input)
  (reduce #'+ (mapcar #'fuel (parse input))))

(defun fuel* (mass)
  (labels ((rec (mass sum)
             (let ((fuel (max 0 (fuel mass))))
               (if (zerop fuel)
                   sum
                   (rec fuel (+ fuel sum))))))
    (rec mass 0)))

(defun part2 (input)
  (reduce #'+ (mapcar #'fuel* (parse input))))
