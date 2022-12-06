;;;; day01.lisp

(in-package #:aoc2022.day01)

(defun parse (input)
  (mapcar (a:compose (a:curry #'mapcar #'parse-integer) #'aoc:lines) (aoc:sections input)))

(defun sum (list)
  (reduce #'+ list))

(defun part1 (input)
  (->> input
    (parse)
    (mapcar #'sum)
    (reduce #'max)))

(defun sorted (list)
  (sort list #'>))

(defun take (n list)
  (loop for x in list repeat n collect x))

(defun part2 (input)
  (->> input
    (parse)
    (mapcar #'sum)
    (sorted)
    (take 3)
    (sum)))
