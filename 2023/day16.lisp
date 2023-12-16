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
