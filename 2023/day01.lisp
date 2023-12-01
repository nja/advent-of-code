;;;; day01.lisp

(in-package :aoc2023.day01)

(defun calibration-value (line)
  (parse-integer (format nil "~d~d"
                         (find-if #'digit-char-p line)
                         (find-if #'digit-char-p line :from-end t))))

(defun part1 (input)
  (reduce #'+ (mapcar #'calibration-value (aoc:lines input))))

(defparameter *replacements*
  '(("one" "1") ("two" "2") ("three" "3")
    ("four" "4") ("five" "5") ("six" "6")
    ("seven" "7") ("eight" "8") ("nine" "9")
    ("ten" "10")))

(defun find-digit (line)
  (let (min digit)
    (flet ((f (x y)
             (let ((m (search x line)))
               (when (and m (or (null min) (< m min)))
                 (setf min m digit y)))))
      (dolist (x *replacements* digit)
        (destructuring-bind (a b) x
          (f a b)
          (f b b))))))


(defun find-last-digit (line)
  (let (max digit)
    (flet ((f (x y)
             (let ((m (search x line :from-end t)))
               (when (and m (or (null max) (> m max)))
                 (setf max m digit y)))))
      (dolist (x *replacements* digit)
        (destructuring-bind (a b) x
          (f a b)
          (f b b))))))


(defun calibration-value2 (line)
  (parse-integer (format nil "~a~a"
                         (find-digit line)
                         (find-last-digit line))))


(defun replace-letters (line)
  (loop for (a b) in *replacements*
        do (setf l (ppcre:regex-replace-all a line b))
        finally (return line)))

(defun replace-letters (line)
  (dolist (x *replacements* line)
    (destructuring-bind (a b) x
      (setf line (ppcre:regex-replace-all a line b)))))

(defun part2 (input)
  (reduce #'+ (mapcar #'calibration-value2
                      (aoc:lines input))))
(defparameter *test2* "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen")