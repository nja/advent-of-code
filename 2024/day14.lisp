;;;; day14.lisp

(in-package :aoc2024.day14)

(defun parse (input rows cols)
  (let ((map (make-array (list rows cols))))
    (mapcar (lambda (integers)
              (apply #'robot map integers))
            (mapcar #'aoc:read-integers (aoc:lines input)))))

(defun robot (map x y dx dy)
  (prog1 (make-instance 'robot :x x :y y :dx dx :dy dy :map map)
    (incf (aref map y x))))

(defclass robot ()
  ((x :initarg :x)
   (y :initarg :y)
   (dx :initarg :dx)
   (dy :initarg :dy)
   (map :initarg :map)))

(defun move (n robot)
  (with-slots (x y dx dy map) robot
    (decf (aref map y x))
    (setf x (mod (+ x (* n dx)) (array-dimension map 1)))
    (setf y (mod (+ y (* n dy)) (array-dimension map 0)))
    (incf (aref map y x)))
  robot)

(defun seconds (n robots)
  (mapc (a:curry #'move n) robots))

(defun quadrant (robot)
  (with-slots (map) robot
    (let ((q (mapcar (lambda (d s)
                       (let ((mid (truncate (array-dimension map d) 2))
                             (v (slot-value robot s)))
                         (cond ((< v mid) 0)
                               ((< mid v) 1))))
                     '(0 1)
                     '(y x))))
      (unless (member nil q)
        q))))

(defun quadrants (robots)
  (remove nil (mapcar #'quadrant robots)))

(defun count-quadrants (robots)
  (let ((counts (make-hash-table :test 'equal)))
    (mapc (lambda (q) (incf (gethash q counts 0)))
          (quadrants robots))
    (a:hash-table-values counts)))

(defun part1 (input)
  (reduce #'* (count-quadrants (seconds 100 (parse input 103 101)))))

(defun heuristic (f threshold robots)
  (let ((seconds 0))
    (lambda ()
      (loop for r = (seconds 1 robots)
            for v = (funcall f r)
            do (incf seconds)
            when (< threshold v)
              return (values seconds v robots)))))

(defun streaks (robots)
  (with-slots (map) (first robots)
    (loop for i below (array-total-size map)
          for x = (row-major-aref map i)
          for s = (if (zerop x)
                      0
                      (1+ (or s 0)))
          count (> s 2))))

(defun part2 (input)
  (funcall (heuristic #'streaks 20 (parse input 103 101))))
