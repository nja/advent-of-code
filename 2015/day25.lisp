;;;; day25.lisp

(in-package :aoc2015.day25)

(defun parse (input)
  (mapcar #'parse-integer (ppcre:all-matches-as-strings "\\d+" input)))

(defun triangle-number (n)
  (loop for i from 1
        sum i
        repeat n))

(defun n (row col)
  (+ (triangle-number (+ (1- row) (1- col))) col))

(defun code (n)
  (loop for code = 20151125 then (rem (* code 252533) 33554393)
        repeat (mod n 16777197)
        finally (return code)))

(defun part1 (input)
  (code (apply #'n (parse input))))
