;;;; day05.lisp

(in-package #:aoc2021.day05)

(defun parse (input)
  (mapcar #'parse-line (aoc:lines input)))

(defun parse-line (line)
  (destructuring-bind (x1 y1 x2 y2)
      (mapcar #'parse-integer (ppcre:split "\\D+" line))
    (list (list x1 y1) (list x2 y2))))

(defun make-grid (coords)
  (let ((max (1+ (reduce #'max (a:flatten coords)))))
    (make-array (list max max) :initial-element #\.)))

(defun orthogonal? (a b)
  (or (eql (first a) (first b))
      (eql (second a) (second b))))

(defun orthogonals (coords)
  (remove-if-not (a:curry #'apply #'orthogonal?) coords))

(defun dir (from to)
  (cond ((< from to) 1)
        ((< to from) -1)
        (t 0)))

(defun line (from to)
  (loop with (fx fy) = from
        with (tx ty) = to
        collect (list fx fy)
        until (and (= fx tx) (= fy ty))
        do (incf fx (dir fx tx))
           (incf fy (dir fy ty))))

(defun lines (coords)
  (mapcar (a:curry #'apply #'line) coords))

(defun mark-step (grid x y)
  (if (integerp (aref grid x y))
      (incf (aref grid x y))
      (setf (aref grid x y) 1)))

(defun mark-line (grid line)
  (mapc (a:curry #'apply #'mark-step grid) line))

(defun mark-lines (grid lines)
  (mapc (a:curry #'mark-line grid) lines)
  grid)

(defun marked-grid (coords)
  (mark-lines (make-grid coords) (lines coords)))

(defun count-intersections (grid)
  (loop for i below (array-total-size grid)
        for x = (row-major-aref grid i)
        count (and (integerp x) (< 1 x))))

(defun part1 (input)
  (count-intersections (marked-grid (orthogonals (parse input)))))

(defun part2 (input)
  (count-intersections (marked-grid (parse input))))
