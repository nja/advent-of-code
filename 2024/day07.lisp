;;;; day07.lisp

(in-package :aoc2024.day07)

(defun parse (input)
  (mapcar #'aoc:read-integers (aoc:lines input)))

(defun calibrate (goal value terms)
  (if (null terms)
      (when (eql goal value)
        goal)
      (or (calibrate goal (+ value (first terms)) (rest terms))
          (calibrate goal (* value (first terms)) (rest terms)))))

(defun calibration-value (f equation)
  (destructuring-bind (goal term . rest) equation
      (or (funcall f goal term rest) 0)))

(defun part1 (input)
  (reduce #'+ (mapcar (a:curry #'calibration-value #'calibrate) (parse input))))

(defun || (a b)
  (flet ((digits (n)
           (if (zerop n)
               1
               (1+ (truncate (log n 10))))))
    (+ (* a (expt 10 (digits b)))
       b)))

(defun calibrate-2 (goal value terms)
  (if (null terms)
      (when (eql goal value)
        goal)
      (or (calibrate-2 goal (+  value (first terms)) (rest terms))
          (calibrate-2 goal (*  value (first terms)) (rest terms))
          (calibrate-2 goal (|| value (first terms)) (rest terms)))))

(defun part2 (input)
  (reduce #'+ (mapcar (a:curry #'calibration-value #'calibrate-2) (parse input))))
