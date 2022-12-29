;;;; day06.lisp

(in-package #:aoc2015.day06)

(defun digits (string)
  (read-from-string (format nil "(~a)" (ppcre:regex-replace-all "\\D+" string " "))))

(defun parse-line (line)
  (cons (cond ((str:starts-with? "turn on" line) 'on)
              ((str:starts-with? "turn off" line) 'off)
              ((str:starts-with? "toggle" line) 'toggle))
        (digits line)))

(defun parse (input)
  (mapcar #'parse-line (aoc:lines input)))

(defparameter *grid* nil)

(defun operate (op x1 y1 x2 y2)
  (loop for row from y1 to y2 do
    (loop for col from x1 to x2
          do (setf (aref *grid* row col)
                   (case op
                     (on t)
                     (off nil)
                     (toggle (not (aref *grid* row col))))))))

(defun count-lit ()
  (loop for i below (array-total-size *grid*)
        count (row-major-aref *grid* i)))

(defun part1 (input)
  (let ((*grid* (make-array '(1000 1000) :initial-element nil)))
    (mapc (a:curry #'apply #'operate) (parse input))
    (count-lit)))

(defun operate2 (op x1 y1 x2 y2)
  (loop with d = (ecase op (on 1) (off -1) (toggle 2))
        for row from y1 to y2 do
          (loop for col from x1 to x2
                do (setf (aref *grid* row col)
                         (max 0 (+ d (aref *grid* row col)))))))

(defun sum-brightness ()
  (loop for i below (array-total-size *grid*)
        sum (row-major-aref *grid* i)))

(defun part2 (input)
  (let ((*grid* (make-array '(1000 1000) :initial-element 0)))
    (mapc (a:curry #'apply #'operate2) (parse input))
    (sum-brightness)))
