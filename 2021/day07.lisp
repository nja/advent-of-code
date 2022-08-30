;;;; day07.lisp

(in-package #:aoc2021.day07)

(defun parse (input)
  (mapcar #'parse-integer (ppcre:split "," input)))

(defun distance (target crab)
  (abs (- target crab)))

(defun part1 (input)
  (let ((crabs (parse input)))
   (reduce #'+ (mapcar (a:curry #'distance (a:median crabs)) crabs))))

(defun cost (distance)
  (/ (* distance (1+ distance)) 2))

(defun evaluate (crabs target)
  (loop with cost = (a:compose #'cost (a:curry #'distance target))
        for crab across crabs
        sum (funcall cost crab)))

(defun minimize-cost (crabs)
  (loop with best
        for target below (reduce #'max crabs)
        for cost = (evaluate crabs target)
        minimize cost into min-cost
        when (= cost min-cost)
          do (setf best target)
        finally (return (values min-cost best))))

(defun to-array (crabs)
  (make-array (length crabs) :initial-contents crabs))

(defun part2 (input)
  (minimize-cost (to-array (parse input))))
