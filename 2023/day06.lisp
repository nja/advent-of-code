;;;; day06.lisp

(in-package :aoc2023.day06)

(defun integers (line)
  (remove nil (mapcar (lambda (s) (parse-integer s :junk-allowed t))
                      (str:split " " line))))

(defun races (input)
  (apply #'mapcar #'list (mapcar #'integers (aoc:lines input))))

(defun ways-to-win (race)
  (destructuring-bind (time distance) race
    (loop for s from 1 below time
          for f = (- time s)
          count (< distance (* s f)))))

(defun part1 (input)
  (reduce #'* (mapcar #'ways-to-win (races input))))

(defun integer* (line)
  (parse-integer (remove-if-not #'digit-char-p line)))

(defun race (input)
  (mapcar #'integer* (aoc:lines input)))

(defun part2 (input)
  (ways-to-win (race input)))
