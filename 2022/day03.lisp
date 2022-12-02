;;;; day03.lisp

(in-package #:aoc2022.day03)

(defun compartments (line)
  (mapcar (a:rcurry #'coerce 'list)
          (list (subseq line 0 (truncate (length line) 2))
                (subseq line (truncate (length line) 2)))))

(defun priority (char)
  (let ((x (char-code char)))
    (if (< x (char-code #\a))
        (+ (priority #\z) (- x (char-code #\A) -1))
        (- x (char-code #\a) -1))))

(defun priority-sum (groups)
  (reduce #'+ (mapcar (lambda (g) (priority (first (reduce #'intersection g)))) groups)))

(defun part1 (input)
  (priority-sum (mapcar #'compartments (aoc:lines input))))

(defun groups (input)
  (loop for x on (aoc:lines input) by #'cdddr
        for lines = (subseq x 0 3)
        collect (mapcar (a:rcurry #'coerce 'list) lines)))

(defun part2 (input)
  (priority-sum (groups input)))
