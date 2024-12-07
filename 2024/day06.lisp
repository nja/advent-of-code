;;;; day06.lisp

(in-package :aoc2024.day06)

(defun pos (x y) (list x y))
(defun add (a b) (mapcar #'+ a b))
(defun sub (a b) (mapcar #'- a b))

(defclass guard ()
  ((position :initarg :position)
   (dir :initarg :dir)
   (map :initarg :map)
   (visited :initarg :visited)
   (obstacle :initarg :obstacle :initform nil)
   (steps :initform 0 :reader steps)))

(defun guard (array &optional starting-pos dir obstacle)
  (make-instance 'guard
                 :position (or starting-pos (starting-pos array))
                 :dir (or dir '(-1 0))
                 :map array
                 :visited (make-array (array-dimensions array) :initial-element nil)
                 :obstacle obstacle))

(defun move (guard)
  (with-slots (position dir array visited) guard
    (when position
      (push dir (apply #'aref visited position))
      (multiple-value-bind (np what) (ahead-of guard)
        (cond ((null np)
               (setf position np))
              ((eql what #\#)
               (turn guard)
               position)
              (t
               (setf position np)))))))

(defun turn (guard)
  (with-slots (dir) guard
    (setf dir (right dir))))

(defun right (d)
  (cond ((equal d '(-1 0)) '(0 1))
        ((equal d '(0 1)) '(1 0))
        ((equal d '(1 0)) '(0 -1))
        ((equal d '(0 -1)) '(-1 0))))

(defun ahead-of (guard)
  (with-slots (map position dir) guard
    (let ((np (add position dir)))
      (if (apply #'array-in-bounds-p map np)
          (values np (apply #'aref map np))
          (values nil nil)))))

(defun starting-pos (array)
  (loop for row below (array-dimension array 0)
        do (loop for col below (array-dimension array 1)
                 when (equal #\^ (aref array row col))
                   do (return-from starting-pos (list row col)))))

(defun patrol (guard)
  (loop while (move guard)
        finally (return guard)))

(defun count-visited (guard)
  (with-slots (visited) guard
    (loop for i below (array-total-size visited)
          count (row-major-aref visited i))))

(defun part1 (input)
  (count-visited (patrol (guard (aoc:to-array input)))))

(defun count-loop-obstructions (map visited)
  (loop for row below (array-dimension visited 0)
        sum (loop for col below (array-dimension visited 1)
                  for pos = (pos row col)
                  for dir = (car (last (aref visited row col)))
                  count (and dir (loop? (guard map (sub pos dir) dir pos))))))

(defun loop? (guard)
  (loop for x = (move2 guard)
        while x
        when (eq x 'loop) return t))

(defun move2 (guard)
  (with-slots (position dir array visited obstacle) guard
    (when position
      (multiple-value-bind (np what) (ahead-of guard)
        (cond ((null np)
               (setf position np))
              ((or (eql what #\#) (equal np obstacle))
               (if (find dir (apply #'aref visited position))
                   (return-from move2 'loop)
                   (push dir (apply #'aref visited position)))
               (turn guard)
               position)
              (t
               (setf position np)))))))

(defun part2 (input)
  (with-slots (map visited) (patrol (guard (aoc:to-array input)))
    (count-loop-obstructions map visited)))
