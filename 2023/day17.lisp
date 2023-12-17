;;;; day17.lisp

(in-package :aoc2023.day17)

(defun parse (input)
  (let ((array (aoc:to-array input)))
    (dotimes (i (array-total-size array) array)
      (setf (row-major-aref array i) (digit-char-p (row-major-aref array i))))))

(defstruct (state :conc-name) row col dir n)

(defun neighbours (array valid-direction? done? &optional min)
  (lambda (state)
    (loop for dir in '(north east south west)
          for next = (make-state :row (+ (row state) (case dir (north -1) (south 1) (t 0)))
                                 :col (+ (col state) (case dir (west -1) (east 1) (t 0)))
                                 :dir dir
                                 :n (if (eq (dir state) dir)
                                               (1+ (n state))
                                               1))
          when (and (funcall valid-direction? (dir state) (n state) dir)
                    (array-in-bounds-p array (row next) (col next))
                    (or (null min)
                        (not (funcall done? next))
                        (<= min (n next))))
            collect next)))

(defun done? (array)
  (destructuring-bind (row col) (array-dimensions array)
    (lambda (state)
      (and (eql (1- row) (row state))
           (eql (1- col) (col state))))))

(defun valid-direction? (from n dir)
  (cond ((null from) t)
        ((eq from dir) (< n 3))
        (t (not (eq dir (case from (north 'south) (east 'west) (south 'north) (west 'east)))))))

(defun distance (array)
  (lambda (src dst)
    (declare (ignore src))
    (aref array (row dst) (col dst))))

(defun least-heat-loss (array neighbours)
  (dijkstra:distance (dijkstra:search* (make-state :row 0 :col 0 :n 0)
                                       neighbours
                                       :distancef (distance array)
                                       :donep (done? array))))

(defun part1 (input)
  (let ((array (parse input)))
    (least-heat-loss array (neighbours array #'valid-direction? (done? array)))))

(defun ultra-valid-direction? (from n dir)
  (cond ((null from) t)
        ((< n 4) (eq from dir))
        ((eq from dir) (< n 10))
        (t (not (eq dir (case from (north 'south) (east 'west) (south 'north) (west 'east)))))))

(defun part2 (input)
  (let ((array (parse input)))
    (least-heat-loss array (neighbours array #'ultra-valid-direction? (done? array) 4))))
