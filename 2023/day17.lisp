;;;; day17.lisp

(in-package :aoc2023.day17)

(defun parse (input)
  (let ((array (aoc:to-array input)))
    (dotimes (i (array-total-size array) array)
      (setf (row-major-aref array i) (digit-char-p (row-major-aref array i))))))

(defparameter *array* (parse *test*))

(defstruct (state (:conc-name s) (:constructor state (row col dir count))) row col dir count)

(defun neighbours (valid-direction? &optional min-count done?)
  (lambda (state)
    (loop for new-dir in '(north east south west)
          for new-row = (case new-dir
                          (north (1- (srow state)))
                          (south (1+ (srow state)))
                          (t (srow state)))
          for new-col = (case new-dir
                          (east (1+ (scol state)))
                          (west (1- (scol state)))
                          (t (scol state)))
          for new-count = (if (eq (sdir state) new-dir)
                              (1+ (scount state))
                              1)
          for new-state = (state new-row new-col new-dir new-count)
          when (and (funcall valid-direction? (sdir state) (scount state) new-dir)
                    (array-in-bounds-p *array* new-row new-col)
                    (or (null min-count)
                        (not (funcall done? new-state))
                        (<= min-count new-count)))
            collect new-state)))


(defun done? (array)
  (destructuring-bind (row col) (array-dimensions array)
    (lambda (state)
      (and (eql (1- row) (srow state))
           (eql (1- col) (scol state))))))

(defun valid-direction? (from count dir)
  (cond ((null from) t)
        ((eq from dir) (< count 3))
        (t (not (eq dir (case from (north 'south) (east 'west) (south 'north) (west 'east)))))))

(defun distance (src dst)
  (declare (ignore src))
  (aref *array* (srow dst) (scol dst)))

(defun least-heat-loss (array neighbours)
  (let* ((*array* array)
         (n (dijkstra:search* (state 0 0 nil 0) neighbours
                              :distancef #'distance
                              :donep (done? array))))
    ;; (loop for x = n then (dijkstra:previous x)
    ;;       while x
    ;;       for state = (dijkstra:item x)
    ;;       when (sdir state) do (setf (aref *array* (srow state) (scol state))
    ;;                                  (case (sdir state)
    ;;                                    (north #\^)
    ;;                                    (east #\>)
    ;;                                    (south #\v)
    ;;                                    (west #\<))))
    ;; (aoc:print-array *array*)
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
