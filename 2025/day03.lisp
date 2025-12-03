;;;; day03.lisp

(in-package :aoc2025.day03)

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

(defun joltage* (bank n)
  (loop for left from (1- n) downto 0
        for (x i) = (multiple-value-list (find-max bank (if i (1+ i) 0) (- (length bank) left)))
        for sum = (+ (* (or sum 0) 10) x)
        finally (return sum)))

(defun part2 (input)
  (reduce #'+ (mapcar (a:rcurry #'joltage* 12) (banks input))))
