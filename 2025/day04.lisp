;;;; day04.lisp

(in-package :aoc2025.day04)

(defun count-neighbours (array what row col)
  (flet ((at (r c) (when (array-in-bounds-p array r c)
                     (aref array r c))))
    (loop for r from (1- row) to (1+ row)
          sum (loop for c from (1- col) to (1+ col)
                    unless (and (= row r) (= col c))
                      count (eql what (at r c))))))

(defun accessible? (array row col)
  (and (char= #\@ (aref array row col))
       (< (count-neighbours array #\@ row col) 4)))

(defun count-accessible (array)
  (loop for row below (array-dimension array 0)
        sum (loop for col below (array-dimension array 1)
                  count (accessible? array row col))))

(defun part1 (input)
  (count-accessible (aoc:to-array input)))

(defun as-counts (array)
  (loop with copy = (a:copy-array array)
        for row below (array-dimension array 0)
        do (loop for col below (array-dimension array 1)
                 when (char= #\@ (aref array row col))
                   do (setf (aref copy row col)
                            (count-neighbours array #\@ row col)))
        finally (return copy)))

(defun remove-roll (array row col)
  (assert (integerp (aref array row col)))
  (setf (aref array row col) #\.)
  (flet ((at (r c) (and (array-in-bounds-p array r c)
                        (aref array r c))))
    (1+ (loop for r from (1- row) to (1+ row)
              sum (progn
                    (loop for c from (1- col) to (1+ col)
                          when (integerp (at r c))
                            do (decf (aref array r c)))
                    (loop for c from (1- col) to (1+ col)
                          sum (or (maybe-remove-roll array r c) 0)))))))

(defun maybe-remove-roll (array row col)
  (when (and (array-in-bounds-p array row col)
             (integerp (aref array row col))
             (< (count-integer-neighbours array row col) 4))
    (remove-roll array row col)))

(defun count-integer-neighbours (array row col)
  (flet ((at (r c) (when (array-in-bounds-p array r c)
                     (aref array r c))))
    (loop for r from (1- row) to (1+ row)
          sum (loop for c from (1- col) to (1+ col)
                    unless (and (= row r) (= col c))
                      count (integerp (at r c))))))

(defun sweep (array)
  (loop for row below (array-dimension array 0)
        sum (loop for col below (array-dimension array 1)
                  sum (or (maybe-remove-roll array row col) 0))))

(defun part2 (input)
  (sweep (as-counts (aoc:to-array input))))
