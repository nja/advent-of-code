;;;; day06.lisp

(in-package :aoc2024.day06)

(defparameter *test*
"....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...")

(defun pos (x y) (list x y))
(defun add (a b) (mapcar #'+ a b))
;(defun turn (d) (getf d '((1 0) (0 1) (-1 0) (0 -1) (1 0))))

(defun left (d)
  (cond ((equal d '(1 0)) '(0 1))
        ((equal d '(0 1)) '(-1 0))
        ((equal d '(-1 0)) '(0 -1))
        ((equal d '(0 -1)) '(1 0))))


(defun right (d)
  (cond ((equal d '(-1 0)) '(0 1))
        ((equal d '(0 1)) '(1 0))
        ((equal d '(1 0)) '(0 -1))
        ((equal d '(0 -1)) '(-1 0))))


(defun move (array p d &optional seen)
  (setf (apply #'aref array p) #\X)
  (when seen
    (if (gethash (list p d) seen)
        (return-from move (list nil t))
        (setf (gethash (list p d) seen) t)))
  (let ((np (add p d)))
    (cond ((not (apply #'array-in-bounds-p array np)) (list nil nil))
          ((equal #\# (apply #'aref array np))
           (list p (right d)))
          (t (list np d)))))

(defun starting-pos (array)
  (loop for row below (array-dimension array 0)
        do (loop for col below (array-dimension array 1)
                 when (equal #\^ (aref array row col))
                   do (return-from starting-pos (list row col)))))

(defun count-c (array c)
  (loop for row below (array-dimension array 0)
        sum (loop for col below (array-dimension array 1)
                 counting (equal c (aref array row col)))))

(defun patrol (array)
  (loop for (p d) = (list (starting-pos array) '(-1 0))
          then (move array p d)
        while p
        finally (return array)))

(defun part1 (input)
  (count-c (patrol (aoc:to-array input)) #\X))

(defun place-block (array row col)
  (let ((copy (a:copy-array array)))
    (setf (aref copy row col) #\#)
    copy))

(defun is-loop? (array op od)
  (let ((seen (make-hash-table :test 'equalp)))
    (loop for (p d) = (list op od)
            then (move array p d seen)
          while p
          finally (return d))))

(defun part2 (input)
  (let* ((array (aoc:to-array input))
         (sp (starting-pos array)))
    (patrol array)
    (loop for row below (array-dimension array 0)
          sum (loop for col below (array-dimension array 1)
                    for x = (aref array row col)
                    count (and (eql x #\X) (is-loop? (place-block array row col) sp '(-1 0)))))))
