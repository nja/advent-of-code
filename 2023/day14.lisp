;;;; day14.lisp

(in-package :aoc2023.day14)

(defparameter *test*
"O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....")

(defun tilt-north (array)
  (loop for col below (array-dimension array 1)
        do (tilt* array 0 1 col 0)))

(defun tilt-south (array)
  (destructuring-bind (rows cols) (array-dimensions array)
    (loop for col below cols
          do (tilt* array (1- rows) -1 col 0))))

(defun tilt-east (array)
  (destructuring-bind (rows cols) (array-dimensions array)
    (loop for row below rows
          do (tilt* array row 0 (1- cols) -1))))

(defun tilt-west (array)
  (loop for row below (array-dimension array 0)
        do (tilt* array row 0 0 1)))

(defun cycles (array n)
  (dotimes (x n array)
    (declare (type fixnum n x))
    (tilt-north array)
    (tilt-west array)
    (tilt-south array)
    (tilt-east array)))

(defun tilt* (array start-row rd start-col cd)
  (flet ((free? (c) (char= #\. c))
         (round? (c) (char= #\O c))
         (square? (c) (char= #\# c)))
    (loop with fr  and fc
          for row = start-row then (+ row rd)
          for col = start-col then (+ col cd)
          while (array-in-bounds-p array row col)
          for c = (aref array row col)
          do (cond ((and fr (round? c))
                    (rotatef (aref array row col) (aref array fr fc))
                    (incf fr rd)
                    (incf fc cd))
                   ((or (round? c) (square? c))
                    (setf fr nil))
                   ((and (not fr) (free? c))
                    (setf fr row
                          fc col))))))

(defun total-load (array)
  (let ((rows (array-dimension array 0)))
    (loop for row below rows
          sum (loop for col below (array-dimension array 1)
                    when (char= #\O (aref array row col))
                      sum (- rows row)))))

(defun part1 (input)
  (let ((array (aoc:to-array input)))
    (tilt-north array)
    (total-load array)))

(defun cycle-hash (array)
  (let ((copy (a:copy-array array)))
    (loop for i from 1
          with hash = (make-hash-table :test 'equalp)
          do (tilt-north copy)
             (tilt-west copy)
             (tilt-south copy)
             (tilt-east copy)
          when (< 100 i)
            do (push i (gethash (a:copy-array copy) hash))
          repeat 200
          finally (return hash))))


(defun load-hash (array)
  (let ((copy (a:copy-array array)))
    (loop for i from 1
          with hash = (make-hash-table)
          do (tilt-north copy)
             (tilt-west copy)
             (tilt-south copy)
             (tilt-east copy)
          do (push i (gethash (total-load copy) hash))
                                        ;until (< 10 (length (gethash (total-load copy) hash)))
             repeat 36
          finally (return hash))))

(defun skip-cycles (hash n)
  (let ((ramp-up (ramp-up hash))
        (cycle-length (cycle-length hash)))
    (+ ramp-up cycle-length (mod (- n ramp-up) cycle-length))))

(defun cycle-length (hash)
  (let ((diffs (remove-duplicates (mapcar (lambda (l) (reduce #'+ (remove-duplicates (mapcar #'- l (cdr l)))))
                                          (remove-if-not #'cdr (a:hash-table-values hash))))))
    (and (= 1 (length diffs)) (first diffs))))

(defun ramp-up (hash)
  (count-if-not #'cdr (a:hash-table-values hash)))

(defun part2 (input)
  (let ((array (aoc:to-array input)))
    (total-load (cycles array (skip-cycles (load-hash array) 1000000000)))))

