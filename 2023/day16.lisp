;;;; day16.lisp

(in-package :aoc2023.day16)

(defparameter *test*
".|...\\....
|.-.\\.....
.....|-...
........|.
..........
.........\\
..../.\\\\..
.-.-/..|..
.|....-|.\\
..//.|....")

(defun to-array (input)
  (loop with array = (make-array (list (length (aoc:lines input))
                                       (length (first (aoc:lines input)))))
        for i from 0
        for c across (remove #\Newline input)
        do (setf (row-major-aref array i) c)
        finally (return array )))

(defun turn-clockwise (direction)
  (getf '(up right right down down left left up) direction))
(defun turn-anti-clockwise (direction)
  (getf '(up left left down down right right up) direction))

(defun reflect (mirror direction)
  (funcall (case mirror
             (#\\ (case direction
                    ((up down) #'turn-anti-clockwise)
                    ((right left) #'turn-clockwise)))
             (#\/ (case direction
                    ((up down) #'turn-clockwise)
                    ((left right) #'turn-anti-clockwise))))
           direction))

(defun split (mirror direction)
  (or (case mirror
        (#\| (case direction ((left right) '(up down))))
        (#\- (case direction ((up down) '(left right)))))
      (list direction)))

(defun move (direction row col)
  (incf row (case direction
              (up -1)
              (down 1)
              (t 0)))
  (incf col (case direction
              (left -1)
              (right 1)
              (t 0)))
  (list direction row col))

(defun beam (beam array energized)
  (destructuring-bind (direction row col) beam
    (when (and (array-in-bounds-p array row col)
               (not (gethash beam energized)))
      (setf (gethash beam energized) t)
      (let ((at (aref array row col)))
        (case at
          (#\. (list (move direction row col)))
          ((#\\ #\/) (list (move (reflect at direction) row col)))
          ((#\- #\|) (mapcar (lambda (d) (move d row col))
                             (split at direction))))))))

(defun energize (beams array energized)
  (when beams
    (energize (mapcan (lambda (b) (beam b array energized)) beams) array energized)))

(defun part1 (input)
  (let ((energized (make-hash-table :test 'equal)))
    (energize '((right 0 0)) (to-array input) energized)
    (length (remove-duplicates (mapcar #'cdr (a:hash-table-keys energized)) :test #'equal))))