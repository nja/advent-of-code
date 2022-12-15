;;;; day09.lisp

(in-package :aoc2015.day09)

(defun distances (input)
  (let ((*package* (symbol-package 'parse))
        (distances (make-hash-table :test 'equal))
        nodes)
    (dolist (line (aoc:lines input))
      (destructuring-bind (a to b = c) (read-from-string (format nil "(~a)" line))
        (declare (ignore to =))
        (print (list line a b c))
        (setf (gethash (key a b) distances) c)
        (pushnew a nodes)
        (pushnew b nodes)))
    (values distances nodes)))

(defun key (a b)
  (if (string< (symbol-name a) (symbol-name b))
      (cons a b)
      (cons b a)))

(defvar *distances*)

(defun distance (a b)
  (gethash (key a b) *distances*))

(defun visit (nodes)
  (if (null (rest nodes))
      0
      (reduce #'min (mapcar (lambda (n)
                              (+ (distance (first nodes) n)
                                 (visit (remove n nodes))))
                            (rest nodes)))))

(defun part1 (input)
  (multiple-value-bind (*distances* nodes) (distances input)
    (visit nodes)))

(defparameter *test* (remove #\Return
"London to Dublin = 464
London to Belfast = 518
Dublin to Belfast = 141"))