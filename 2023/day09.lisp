;;;; day09.lisp

(in-package :aoc2023.day09)

(defun parse (input)
  (mapcar (a:compose (a:curry #'mapcar #'parse-integer)
                     (a:curry #'str:split " "))
          (aoc:lines input)))

(defun differences (numbers)
  (mapcar (lambda (a b) (- b a)) numbers (cdr numbers)))

(defun predictions (numbers)
  (let ((differences (differences numbers)))
    (if (every #'zerop differences)
        (values (a:lastcar numbers) (first numbers))
        (multiple-value-bind (last first) (predictions differences)
          (values (+ (a:lastcar numbers) last) (- (first numbers) first))))))

(defun part1 (input)
  (reduce #'+ (mapcar #'prediction (parse input))))

(defun part2 (input)
  (reduce #'+ (mapcar (lambda (n) (nth-value 1 (predictions n))) (parse input))))

(defun prediction (numbers)
  (let ((differences (differences numbers)))
    (if (every #'zerop differences)
        (a:lastcar numbers)
        (+ (a:lastcar numbers) (prediction differences)))))
