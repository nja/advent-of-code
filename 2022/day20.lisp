;;;; day20.lisp

(in-package :aoc2022.day20)

(defstruct (node :conc-name)
  "Node for a double linked list."
  next prev n)

(defun numbers (input &optional (factor 1))
  (with-input-from-string (in input)
    (loop while (listen in)
          collect (* (read in) factor) into numbers
          count 1 into count
          finally (return (values (circular numbers) count)))))

(defun circular (numbers)
  (cdr (rplacd (last numbers) numbers)))

(defun cut (numbers)
  (prog1 (cdr numbers)
    (rplacd numbers (cddr numbers))))

(defun original-order (numbers)
  (loop for n on numbers
        collect n
        until (eq (cdr n) numbers)))

(defparameter *count* nil)

(defun nth-number (numbers n)
  (let ((n (mod n *count*)))
    (case (signum n)
      (0 numbers)
      (1 (nth-number (cdr numbers) (1- n)))
      (-1 (nth-number numbers n)))))

(defun insert-after (place number)
  (rplacd number (cdr place))
  (rplacd place number)
  number)

(defun move (number)
  (let* ((n (mod (car number) (1- *count*)))
         (before (nth-number number -1)))
    (cut before)
    (insert-after (nth-number before (if (plusp n) n (1- n))) number)))

(defun mix (numbers &optional (times 1))
  (let ((order (original-order numbers)))
    (dotimes (i times numbers)
      (dolist (number order)
        (move number)))))

(defun find-number (numbers n)
  (if (eql n (car numbers))
      numbers
      (find-number (cdr numbers) n)))

(defun grove-coords (numbers)
  (let ((zero (find-number numbers 0)))
    (+ (car (nth-number zero 1000))
       (car (nth-number zero 2000))
       (car (nth-number zero 3000)))))

(defun part1 (input)
  (multiple-value-bind (numbers *count*) (numbers input)
    (grove-coords (mix numbers))))

(defun part2 (input)
  (multiple-value-bind (numbers *count*) (numbers input 811589153)
    (grove-coords (mix numbers 10))))
