;;;; day03.lisp

(in-package #:aoc2015.day03)

(defstruct (pos :conc-name (:constructor pos (x y))) x y)

(defun move (pos dir)
  (case dir
    (#\^ (pos (x pos) (1- (y pos))))
    (#\> (pos (1+ (x pos)) (y pos)))
    (#\v (pos (x pos) (1+ (y pos))))
    (#\< (pos (1- (x pos)) (y pos)))))

(defun track (input)
  (let ((track (make-hash-table :test 'equalp)))
    (flet ((track (pos) (incf (gethash pos track 0)) pos))
     (reduce (a:compose #'track #'move) input :initial-value (track (pos 0 0))))
    track))

(defun part1 (input)
  (hash-table-count (track input)))

(defun robo-track (input)
  (let ((track (make-hash-table :test 'equalp)))
    (flet ((track (pos) (incf (gethash pos track 0)) pos))
      (loop with a = (track (pos 0 0)) and b = (track (pos 0 0))
            for dir across input
            do (setf a (track (move a dir)))
               (rotatef a b)))
    track))

(defun part2 (input)
  (hash-table-count (robo-track input)))
