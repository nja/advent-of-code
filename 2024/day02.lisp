;;;; day02.lisp

(in-package :aoc2024.day02)

(defun reports (input)
  (mapcar (lambda (line) (read-from-string (format nil "(~a)" line))) (aoc:lines input)))

(defun safe? (levels)
  (let ((diffs (mapcar #'- levels (rest levels))))
    (and (every (a:curry #'eq (signum (first diffs)))
                (mapcar #'signum diffs))
         (every (lambda (x) (<= 1 (abs x) 3)) diffs))))

(defun part1 (input)
  (count-if #'safe? (reports input)))

(defun remove-level (report i)
  (append (subseq report 0 i) (subseq report (1+ i))))

(defun safe-when-dampened? (levels)
  (loop for i below (length levels)
          thereis (safe? (remove-level levels i))))

(defun part2 (input)
  (count-if #'safe-when-dampened? (reports input)))
