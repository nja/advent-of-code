;;;; day16.lisp

(in-package :aoc2023.day16)

(defun turn-clockwise (direction)
  (case direction (up 'right) (right 'down) (down 'left) (left 'up)))

(defun turn-anti-clockwise (direction)
  (case direction (up 'left) (left 'down) (down 'right) (right 'up)))

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

(defun count-energized (array beam)
  (let ((energized (make-hash-table :test 'equal)))
    (energize (list beam) array energized)
    (length (remove-duplicates (mapcar #'cdr (a:hash-table-keys energized)) :test #'equal))))

(defun part1 (input)
  (count-energized (aoc:to-array input) '(right 0 0)))

(defun edge-beams (rows cols)
  (append (loop for row below rows
                collect (list 'right row 0)
                collect (list 'left row (1- cols)))
          (loop for col from 0 below (1- cols)
                collect (list 'up (1- rows) col)
                collect (list 'down 0 col))))

(defun part2 (input)
  (let ((array (aoc:to-array input)))
    (reduce #'max (mapcar (a:curry #'count-energized array)
                          (apply #'edge-beams (array-dimensions array))))))
