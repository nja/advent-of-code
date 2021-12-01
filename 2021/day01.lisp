(in-package #:aoc2021.day01)

(defun parse (input)
  (mapcar #'parse-integer (aoc:lines input)))

(defun predicate2int (p)
  (lambda (&rest args)
    (if (apply p args) 1 0)))

(defun offsets (n list)
  (loop repeat n
        for x on list
        collect x))

(defun count-increases (list)
  (reduce #'+ (apply #'mapcar (predicate2int #'<) (offsets 2 list))))

(defun part1 (input)
  (count-increases (parse input)))

(defun windows (n list)
  (apply #'mapcar #'list (offsets n list)))

(defun sums (windows)
  (mapcar (a:curry #'apply #'+) windows))

(defun part2 (input)
  (count-increases (sums (windows 3 (parse input)))))
