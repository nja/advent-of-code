;;;; day13.lisp

(in-package :aoc2015.day13)

(defun parse (input)
  (let ((opinions (make-hash-table :test 'equal))
        names)
    (dolist (line (aoc:lines input) (values opinions names))
      (ppcre:register-groups-bind (a f n b)
          ("(\\w+) would (lose|gain) (\\d+) happiness units by sitting next to (\\w+)\\." line)
        (let ((a (intern a (symbol-package 'parse)))
              (b (intern b (symbol-package 'parse)))
              (f (if (equal f "gain") 1 -1))
              (n (parse-integer n)))
          (incf (gethash (key a b) opinions 0) (* n f))
          (pushnew a names)
          (pushnew b names))))))

(defun key (a b)
  (if (string< (symbol-name a) (symbol-name b))
      (cons a b)
      (cons b a)))

(defun score (opinions seating)
  (loop for (a b) on seating
        sum (gethash (key a (or b (first seating))) opinions 0)))

(defun best-score (opinions names)
  (let (best)
    (a:map-permutations
     (lambda (seating)
       (let ((score (score opinions seating)))
         (setf best (max score (or best score)))))
     names)
    best))

(defun part1 (input)
  (multiple-value-call #'best-score (parse input)))

(defun part2 (input)
  (multiple-value-bind (opinions names) (parse input)
      (best-score opinions (cons 'myself names))))
