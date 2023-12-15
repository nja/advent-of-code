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

(defun to-array (input)
  (loop with array = (make-array (list (length (aoc:lines input))
                                       (length (first (aoc:lines input)))))
        for i from 0
        for c across (remove #\Newline input)
        do (setf (row-major-aref array i) c)
        finally (return array)))

(defun tilt-north (array rows cols)
  (declare (type fixnum rows cols))
  (loop for col below cols
        do (tilt* array 0 1 col 0 rows cols)))

(defun tilt-south (array rows cols)
  (declare (type fixnum rows cols))
  (loop for col below cols
        do (tilt* array (1- rows) -1 col 0 rows cols)))

(defun tilt-east (array rows cols)
  (declare (type fixnum rows cols))
  (loop for row below rows
        do (tilt* array row 0 (1- cols) -1 rows cols)))

(defun tilt-west (array rows cols)
  (declare (type fixnum rows cols))
  (loop for row below rows
        do (tilt* array row 0 0 1 rows cols)))

(defun cycles (array n)
  (let ((rows (array-dimension array 0))
        (cols (array-dimension array 1)))
    (dotimes (x n array)
      (declare (type fixnum n x))
      (tilt-north array rows cols)
      (tilt-west array rows cols)
      (tilt-south array rows cols)
      (tilt-east array rows cols))))

(defun tilt* (array start-row rd start-col cd rows cols)
  (declare (type fixnum start-row rd start-col cd rows cols))
  (flet ((free? (c) (char= #\. c))
         (round? (c) (char= #\O c))
         (square? (c) (char= #\# c)))
    (loop with free and fr fixnum  and fc fixnum
          for row = start-row then (+ row rd)
          for col = start-col then (+ col cd)
          while (and (<= 0 row (1- rows))
                     (<= 0 col (1- cols)))
          for c = (aref array row col)
          do (cond ((and free (round? c))
                    (rotatef (aref array row col) (aref array fr fc))
                    (incf fr rd)
                    (incf fc cd))
                   ((or (round? c) (square? c))
                    (setf free nil))
                   ((and (not free) (free? c))
                    (setf free t
                          fr row
                          fc col))))))

(defun total-load (array)
  (let ((rows (array-dimension array 0)))
    (loop for row below rows
          sum (loop for col below (array-dimension array 1)
                    when (char= #\O (aref array row col))
                      sum (- rows row)))))

(defun part1 (input)
  (let ((array (to-array input)))
    (tilt-north array (array-dimension array 0) (array-dimension array 1))
    (total-load array)))

(defun load-hash (array)
  (let ((copy (a:copy-array array))
        (rows (array-dimension array 0))
        (cols (array-dimension array 1)))
    (loop for i from 1
          with hash = (make-hash-table)
          do (tilt-north copy rows cols)
             (tilt-west copy rows cols)
             (tilt-south copy rows cols)
             (tilt-east copy rows cols)
          do (push i (gethash (total-load copy) hash))
          repeat 128
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
  (let ((array (to-array input)))
    (total-load (cycles array (skip-cycles (load-hash array) 1000000000)))))

