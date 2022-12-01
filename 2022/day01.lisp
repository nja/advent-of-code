;;;; day01.lisp

(in-package #:aoc2022.day01)

(defun parse (input)
  (mapcar (a:compose (a:curry #'mapcar #'parse-integer) #'aoc:lines) (aoc:sections input)))

(defun part1 (input)
  (reduce #'max (mapcar (a:curry #'reduce #'+) (parse input))))

(defun part2 (input)
  (reduce #'+ (subseq (sort (mapcar (a:curry #'reduce #'+) (parse input)) #'>) 0 3)))
