;;;; day03.lisp

(in-package :aoc2025.day03)

(defparameter *test*
"987654321111111
811111111111119
234234234234278
818181911112111")

(defun banks (input)
  (mapcar (lambda (digits) (map 'vector #'digit-char-p digits))
          (aoc:lines input)))

(defun find-max (bank start end)
  (loop with max and pos
        for i from start below end
        for x = (aref bank i)
        when (or (null max) (< max x))
          do (setf pos i
                   max x)
        finally (return (values max pos))))

(defun joltage (bank)
  (multiple-value-bind (x i) (find-max bank 0 (1- (length bank)))
    (+ (* x 10) (find-max bank (1+ i) (length bank)))))

(defun part1 (input)
  (reduce #'+ (mapcar #'joltage (banks input))))

;;; 17443

