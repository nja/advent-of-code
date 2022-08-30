;;;; day13.lisp

(in-package #:aoc2021.day13)

(defun coords (input)
  (mapcar #'coord (aoc:lines (first (aoc:sections input)))))

(defun coord (line)
  (mapcar #'parse-integer (str:split #\, line)))

(defun folds (input)
  (mapcar #'parse-fold (aoc:lines (second (aoc:sections input)))))

(defun parse-fold (line)
  (ppcre:register-groups-bind (((aoc:symbols '(x y)) dimension) (#'parse-integer n))
      ("fold along (x|y)=(\\d+)" line)
    (list dimension n)))

(defun fold (coord fold)
  (destructuring-bind (dimension line) fold
    (flet ((f (n) (min n (- (* line 2) n))))
      (destructuring-bind (x y) coord
        (case dimension
          (x (list (f x) y))
          (y (list x (f y))))))))

(defun fold-coords (coords folds)
  (mapcar (lambda (x) (reduce #'fold folds :initial-value x)) coords))

(defun part1 (input)
  (length (remove-duplicates (fold-coords (coords input) (subseq (folds input) 0 1)) :test #'equal)))

(defun coords-max (coords)
  (reduce (a:curry #'mapcar #'max) coords))

(defun print-coords (coords)
  (terpri)
  (destructuring-bind (cols rows) (coords-max coords)
    (loop for y upto rows do
      (loop for x upto cols do
        (princ (if (member (list x y) coords :test #'equal) #\# #\Space)))
      (terpri))))

(defun part2 (input)
  (with-output-to-string (*standard-output*)
    (print-coords (fold-coords (coords input) (folds input)))))
