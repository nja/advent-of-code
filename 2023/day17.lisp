;;;; day17.lisp

(in-package :aoc2023.day17)

(defun parse (input)
  (let ((array (aoc:to-array input)))
    (dotimes (i (array-total-size array) array)
      (setf (row-major-aref array i) (digit-char-p (row-major-aref array i))))))

(defparameter *array* nil)

(defstruct (state :conc-name) row col dir dircount)

(defun neighbours (valid-direction? &optional min-count done?)
  (lambda (state)
    (loop for dir in '(north east south west)
          for next = (make-state :row (+ (row state) (case dir (north -1) (south 1) (t 0)))
                                 :col (+ (col state) (case dir (west -1) (east 1) (t 0)))
                                 :dir dir
                                 :dircount (if (eq (dir state) dir)
                                               (1+ (dircount state))
                                               1))
          when (and (funcall valid-direction? (dir state) (dircount state) dir)
                    (array-in-bounds-p *array* (row next) (col next))
                    (or (null min-count)
                        (not (funcall done? next))
                        (<= min-count (dircount next))))
            collect next)))


(defun done? (array)
  (destructuring-bind (row col) (array-dimensions array)
    (lambda (state)
      (and (eql (1- row) (row state))
           (eql (1- col) (col state))))))

(defun valid-direction? (from count dir)
  (cond ((null from) t)
        ((eq from dir) (< count 3))
        (t (not (eq dir (case from (north 'south) (east 'west) (south 'north) (west 'east)))))))

(defun distance (src dst)
  (declare (ignore src))
  (aref *array* (row dst) (col dst)))

(defun least-heat-loss (array neighbours)
  (let* ((*array* array)
         (n (dijkstra:search* (make-state :row 0 :col 0 :dircount 0)
                              neighbours
                              :distancef #'distance
                              :donep (done? array))))
    (dijkstra:distance n)))

(defun part1 (input)
  (least-heat-loss (parse input) (neighbours #'valid-direction?)))

(defun ultra-valid-direction? (from count dir)
  (cond ((null from) t)
        ((< count 4) (eq from dir))
        ((eq from dir) (< count 10))
        (t (not (eq dir (case from (north 'south) (east 'west) (south 'north) (west 'east)))))))

(defun part2 (input)
  (least-heat-loss (parse input) (neighbours #'ultra-valid-direction? 4 (done? (parse input) ))))
