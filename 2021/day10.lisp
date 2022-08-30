;;;; day10.lisp

(in-package #:aoc2021.day10)

(defparameter *chunks* '((#\( #\)     3 1)
                         (#\[ #\]    57 2)
                         (#\{ #\}  1197 3)
                         (#\< #\> 25137 4)))

(defun chunk (c &optional (key #'first))
  (find c *chunks* :key key))

(defun opener? (x)
  (first (chunk x)))

(defun closer (x)
  (second (chunk x)))

(defun consume (line)
  (loop with stack
        for i from 0
        for x across line
        while (cond ((opener? x) (push (closer x) stack))
                    ((eql x (car stack)) (pop stack)))
        finally (return (if (< i (length line)) x stack))))

(defun corrupt? (x)
  (characterp x))

(defun score-corrupt (character)
  (third (chunk character #'second)))

(defun results (input keepersp score)
  (mapcar score (remove-if-not keepersp (mapcar #'consume (aoc:lines input)))))

(defun part1 (input)
  (reduce #'+ (results input #'corrupt? #'score-corrupt)))

(defun incomplete? (x)
  (consp x))

(defun score-incomplete (stack)
  (reduce (lambda (total score) (+ (* total 5) score))
          (mapcar (lambda (c) (fourth (chunk c #'second))) stack)
          :initial-value 0))

(defun part2 (input)
  (a:median (sort (results input #'incomplete? #'score-incomplete) #'<)))
