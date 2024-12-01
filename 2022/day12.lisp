;;;; day12.lisp

(in-package :aoc2022.day12)

(defun to-array (input)
  (loop with rows = (count #\Newline input)
        with cols = (position #\Newline input)
        with array = (make-array (list rows cols))
        for i from 0
        for c across (remove #\Newline input)
        do (setf (row-major-aref array i) c)
        finally (return array)))

(defun add (a b) (mapcar #'+ a b))

(defparameter *grid* nil)

(defun in-bounds? (pos)
  (apply #'array-in-bounds-p *grid* pos))

(defun height (pos)
  (let ((c (apply #'aref *grid* pos)))
    (char-code (case c
                 (#\S #\a)
                 (#\E #\z)
                 (t c)))))

(defun at-most-one-higher? (a b)
  (<= (- b a) 1))

(defun neighbours (test)
  (lambda (pos)
    (remove-if-not (lambda (n) (and (in-bounds? n) (funcall test (height pos) (height n))))
                   (mapcar (a:curry #'add pos) '((1 0) (-1 0) (0 1) (0 -1))))))

(defun position-of (x)
  (loop for i below (array-total-size *grid*)
        for c = (row-major-aref *grid* i)
        when (eql x c)
          do (return (multiple-value-list (truncate i (array-dimension *grid* 1))))))

(defun part1 (input)
  (let ((*grid* (to-array input)))
    (d:distance (d:search* (position-of #\S)
                           (neighbours #'at-most-one-higher?)
                           :donep (a:curry #'equal (position-of #\E))))))

(defun a? (pos)
  (eql (char-code #\a) (height pos)))

(defun part2 (input)
  (let ((*grid* (to-array input)))
    (d:distance (d:search* (position-of #\E)
                           (neighbours (lambda (a b) (at-most-one-higher? b a)))
                           :donep #'a?))))
