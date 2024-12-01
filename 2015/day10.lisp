;;;; day10.lisp

(in-package :aoc2015.day10)

(defun parse (string)
  (loop for c across (remove #\Newline string)
        for result = (add result 1 (digit-char-p c))
        finally (return (nreverse result))))

(defun add (r n d)
  (if (eql d (cadar r))
      (incf (caar r) n)
      (push (list n d) r))
  r)

(defun look-and-say (runs)
  (loop with result
        for (n d) in runs
        do (setf result (add result 1 n))
           (setf result (add result 1 d))
        finally (return (nreverse result))))

(defun process (runs times)
  (dotimes (i times runs)
    (setf runs (look-and-say runs))))

(defun part1 (input)
  (* 2 (length (process (parse input) (1- 40)))))

(defun part2 (input)
  (* 2 (length (process (parse input) (1- 50)))))
