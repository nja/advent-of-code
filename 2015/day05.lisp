;;;; day05.lisp

(in-package #:aoc2015.day05)

(defun three-vowels? (string)
  (< 2 (ppcre:count-matches "[aeiou]" string)))

(defun twice-in-a-row? (string)
  (ppcre:scan "(.)\\1" string))

(defun forbidden? (string)
  (ppcre:scan "ab|cd|pq|xy" string))

(defun nice? (string)
  (and (three-vowels? string)
       (twice-in-a-row? string)
       (not (forbidden? string))))

(defun part1 (input)
  (count-if #'nice? (aoc:lines input)))

(defun pair-twice? (string)
  (ppcre:scan "(..).*\\1" string))

(defun repeat-with-one-between? (string)
  (ppcre:scan "(.).\\1" string))

(defun nice2? (string)
  (and (pair-twice? string)
       (repeat-with-one-between? string)))

(defun part2 (input)
  (count-if #'nice2? (aoc:lines input)))
