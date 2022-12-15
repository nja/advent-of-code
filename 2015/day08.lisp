;;;; day08.lisp

(in-package :aoc2015.day08)

(defun code-count (string)
  (length string))

(defun memory-count (string)
  (with-input-from-string (*standard-input* string :start 1)
    (flet ((skip (n)
             (dotimes (i n)
               (read-char))))
      (loop for c = (read-char)
            until (char= #\" c)
            count 1
            when (and (char= #\\ c) (char= #\x (read-char)))
              do (skip 2)))))

(defun part1 (input)
  (- (reduce #'+ (mapcar #'code-count (aoc:lines input)))
     (reduce #'+ (mapcar #'memory-count (aoc:lines input)))))

(defun encoded-count (string)
  (+ 2 (loop for c across string
             sum (case c
                   (#\\ 2)
                   (#\" 2)
                   (t 1)))))

(defun part2 (input)
  (- (reduce #'+ (mapcar #'encoded-count (aoc:lines input)))
     (reduce #'+ (mapcar #'code-count (aoc:lines input)))))
