;;;; day04.lisp

(in-package #:aoc2022.day04)

(defun parse-pair (line)
  (read-from-string (format nil "(~a)" (aoc:tr ",-" "  " line))))

(defun fully-contained? (a b c d)
  (or (<= a c d b) (<= c a b d)))

(defun part1 (input)
  (count-if (a:curry #'apply #'fully-contained?) (mapcar #'parse-pair (aoc:lines input))))

(defun overlaps? (a b c d)
  (not (or (< b c) (< d a))))

(defun part2 (input)
  (count-if (a:curry #'apply #'overlaps?) (mapcar #'parse-pair (aoc:lines input))))
