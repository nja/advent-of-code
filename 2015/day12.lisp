;;;; day12.lisp

(in-package :aoc2015.day12)

(defun sum (x)
  (cond ((integerp x) x)
        ((consp x) (+ (sum (car x)) (sum (cdr x))))
        (t 0)))

(defun part1 (input)
  (sum (jsown:parse input)))

(defun skip? (x)
  (and (consp x) (eq :obj (car x))
       (find "red" (cdr x) :key #'cdr :test #'equal)))

(defun sum2 (x)
  (cond ((integerp x) x)
        ((skip? x) 0)
        ((consp x) (+ (sum2 (car x)) (sum2 (cdr x))))
        (t 0)))

(defun part2 (input)
  (sum2 (jsown:parse input)))
