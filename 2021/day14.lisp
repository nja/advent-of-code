;;;; day14.lisp

(in-package #:aoc2021.day14)

(defun elements (input)
  (parse-elements (first (aoc:lines input))))

(defun parse-elements (line)
  (map 'list (lambda (c) (intern (format nil "~a" c) (find-package '#:aoc2021.day14))) line))

(defun rules (input)
  (mapcar #'parse-rule (aoc:lines (second (aoc:sections input)))))

(defun parse-rule (line)
  (destructuring-bind (from (to)) (mapcar #'parse-elements (str:split " -> " line))
    (cons from to)))

(defun max-diff (counts)
  (let ((nums (mapcar #'cdr counts)))
    (- (reduce #'max nums) (reduce #'min nums))))

(defparameter *rules* nil)

(defun combine (a b)
  (cdr (assoc (list a b) *rules* :test #'equal)))

(defun pair-count (n left right)
  (if (or (zerop n) (null right))
      (list (cons left 1))
      (triple-count n left (combine left right) right)))

(defun triple-count (n left middle right)
  (add-counts
   (pair-count (1- n) left middle)
   (pair-count (1- n) middle right)))

(defun add-counts (c1 c2)
  (let ((sums (copy-tree c1)))
    (mapc (lambda (x)
            (incf (cdr (or (assoc (car x) sums)
                           (car (push (cons (car x) 0) sums))))
                  (cdr x)))
          c2)
    sums))

(defun pairs (elements)
  (mapcar #'list elements (append (cdr elements) (list nil))))

(defun steps (n elements)
  (reduce #'add-counts (mapcar (a:curry #'apply #'pair-count n) (pairs elements))))

(defun part1 (input)
  (let ((*rules* (rules input)))
    (max-diff (steps 10 (elements input)))))

(fare-memoization:memoize 'pair-count)

(defun part2 (input)
  (let ((*rules* (rules input)))
    (max-diff (steps 40 (elements input)))))
