;;;; day02.lisp

(in-package #:aoc2015.day02)

(defun parse (line)
  (read-from-string (format nil "(~a)" (substitute #\Space #\x line))))

(defun surface-area (l w h)
  (+ (* 2 l w) (* 2 w h) (* 2 h l)))

(defun slack (l w h)
  (min (* l w) (* w h) (* h l)))

(defun paper (l w h)
  (+ (surface-area l w h) (slack l w h)))

(defun part1 (input)
  (reduce #'+ (mapcar (a:curry #'apply #'paper)
                      (mapcar #'parse (aoc:lines input)))))

(defun smallest-2 (&rest numbers)
  (subseq (sort numbers #'<) 0 2))

(defun smallest-perimiter (l w h)
  (* 2 (reduce #'+ (smallest-2 l w h))))

(defun volume (l w h)
  (* l w h))

(defun ribbon (l w h)
  (+ (smallest-perimiter l w h) (volume l w h)))

(defun part2 (input)
  (reduce #'+ (mapcar (a:curry #'apply #'ribbon)
                      (mapcar #'parse (aoc:lines input)))))
