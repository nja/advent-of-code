;;;; day13.lisp

(in-package #:aoc2016.day13)

(defparameter *input* 10)

(defun open? (pos)
  (let ((x (x pos))
        (y (y pos)))
    (and (not (minusp x))
         (not (minusp y))
         (evenp (logcount (+ (* x x)
                             (* 3 x)
                             (* 2 x y)
                             y
                             (* y y)
                             *input*))))))

(defstruct (pos :conc-name (:constructor pos (x y))) x y)

(defun add (a b)
  (pos (+ (x a) (x b)) (+ (y a) (y b))))

(defparameter *directions* (list (pos 0 -1) (pos 0 1) (pos -1 0) (pos 1 0)))

(defun neighbours (pos)
  (remove-if-not #'open? (mapcar (a:curry #'add pos) *directions*)))

(defun shortest-path (from to)
  (d:distance (d:search* from #'neighbours :donep (a:curry #'equalp to))))

(defun part1 (input)
  (let ((*input* (parse-integer input)))
    (shortest-path (pos 1 1) (pos 31 39))))

(defun reachable-locations (from max-steps)
  (length (nth-value 1 (d:search* from #'neighbours
                                  :max-distance max-steps
                                  :pathsp t))))

(defun part2 (input)
  (let ((*input* (parse-integer input)))
    (reachable-locations (pos 1 1) 50)))
