;;;; day01.lisp

(in-package :aoc2024.day01)

(defun parse (input)
  (let ((pairs (mapcar (lambda (line) (read-from-string (format nil "(~a)" line))) (aoc:lines input))))
    (list (mapcar #'first pairs) (mapcar #'second pairs))))

(defun pair-up (lists)
  (mapcar (lambda (l) (sort l #'<)) lists))

(defun how-far-apart (a b)
  (mapcar (lambda (a b) (abs (- a b))) a b))

(defun part1 (input)
  (reduce #'+ (apply #'how-far-apart (pair-up (parse input)))))

(defun similarity (n list)
  (* n (count n list)))

(defun similarity-score (left right)
  (mapcar (a:rcurry #'similarity right) left))

(defun part2 (input)
  (reduce #'+ (apply #'similarity-score (parse input))))
