;;;; day14.lisp

(in-package :aoc2019.day14)

(defun parse (input)
  (flet ((read-plist (s)
           (reverse (read-from-string (format nil "(~a)" (aoc:tr "," "  " s))))))
    (mapcar (lambda (line)
              (mapcar #'read-plist (str:split " => " line)))
            (aoc:lines input))))

(defun add (bag stuff)
  (cond ((null stuff) bag)
        (t (incf (gethash (first stuff) bag 0) (second stuff))
           (add bag (cddr stuff)))))

(defun take (bag stuff)
  (cond ((null stuff) bag)
        (t (assert (<= 0 (decf (gethash (first stuff) bag 0) (second stuff))))
           (take bag (cddr stuff)))))

(defun have? (bag stuff)
  (or (null stuff)
      (and (<= (gethash (first stuff) bag 0) (second stuff))
           (have? bag (cddr stuff))
           bag)))
