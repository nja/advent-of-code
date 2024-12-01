;;;; day20.lisp

(in-package :aoc2015.day20)

(defun divisors (x)
  (loop for n from 1 upto (sqrt x)
        for (m r) = (multiple-value-list (truncate x n))
        when (zerop r)
          collect n and collect m))

(defun presents (x)
  (* 10 (reduce #'+ (remove-duplicates (divisors x)))))

(defun lowest-house-number (presents f)
  (loop for i from (truncate presents 50)
        when (<= presents (funcall f i))
          return i))

(defun part1 (input)
  (lowest-house-number (parse-integer input) #'presents))

(defun presents2 (x)
  (let ((elves (remove-duplicates
                (remove-if (lambda (d) (< (* 50 d) x))
                           (divisors x)))))
    (* 11 (reduce #'+ elves))))

(defun part2 (input)
  (lowest-house-number (parse-integer input) #'presents2))
