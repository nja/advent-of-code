;;;; day08.lisp

(in-package :aoc2023.day08)

(setf *print-circle* t)

(defun parse-instructions (input)
  (map 'list (lambda (c) (case c (#\L 'L) (#\R 'R))) (first (aoc:sections input))))

(defun parse-map (input)
  (mapcar (lambda (line)
            (mapcar (lambda (s) (intern s (symbol-package 'parse-map)))
                    (ppcre:all-matches-as-strings "[A-Z]+" line)))
          (aoc:lines (second (aoc:sections input)))))

(defun hash-map (map)
  (let ((hash-table (make-hash-table)))
    (dolist (entry map hash-table)
      (setf (gethash (car entry) hash-table) (cdr entry)))))

(defun walk (instructions hash-map)
  (loop with directions = (apply #'a:circular-list instructions)
        for dir in directions
        for steps from 1
        for (l r) = (gethash 'AAA hash-map) then (gethash position hash-map)
        for position = (case dir
                         (l l)
                         (r r))
;        do (break)
        when (eql position 'ZZZ)
          return steps
        repeat 999999))

(defun part1 (input)
  (walk (parse-instructions input) (hash-map (parse-map input))))

(defparameter *test*
"LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)")