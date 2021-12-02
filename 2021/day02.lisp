;;;; day02.lisp

(in-package #:aoc2021.day02)

(defun parse-coords (line)
  (ppcre:register-groups-bind (dir (#'parse-integer dist))
      ("(forward|down|up) ([0-9]+)" line)
    (a:switch (dir :test #'equal)
      ("forward" (list dist 0))
      ("up" (list 0 (- dist)))
      ("down" (list 0 dist)))))

(defun part1 (input)
  (reduce #'* (reduce (a:curry #'mapcar #'+)
                      (mapcar #'parse-coords (aoc:lines input)))))

(defun parse-aim (line)
  (ppcre:register-groups-bind (((aoc:symbols '(forward down up)) dir)
                               (#'parse-integer dist))
      ("(forward|down|up) ([0-9]+)" line)
    (list dir dist)))

(defun aim (state command)
  (destructuring-bind (x y aim) state
    (destructuring-bind (f arg) command
      (case f
        (down
         (incf aim arg))
        (up
         (decf aim arg))
        (forward
         (incf x arg)
         (incf y (* aim arg)))))
    (list x y aim)))

(defun part2 (input)
  (reduce #'* (butlast (reduce #'aim (mapcar #'parse-aim (aoc:lines input))
                               :initial-value (list 0 0 0)))))
