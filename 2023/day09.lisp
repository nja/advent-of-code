;;;; day09.lisp

(in-package :aoc2023.day09)

(defun parse-line (line)
  (mapcar #'parse-integer (str:split " " line)))

(defun parse (input)
  (mapcar #'parse-line (aoc:lines input)))

(defparameter *test*
"0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45")

(defun differences (numbers)
  (mapcar (lambda (a b) (- b a)) numbers (cdr numbers)))

(defun prediction (numbers)
  (let ((differences (differences numbers)))
    (if (every #'zerop differences)
        (a:lastcar numbers)
        (+ (a:lastcar numbers) (prediction differences)))))

(defun part1 (input)
  (reduce #'+ (mapcar #'prediction (parse input))))