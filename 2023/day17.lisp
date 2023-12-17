;;;; day17.lisp

(in-package :aoc2023.day17)

(defparameter *test*
"2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533")

(defun parse (input)
  (let ((array (aoc:to-array input)))
    (dotimes (i (array-total-size array) array)
      (setf (row-major-aref array i) (digit-char-p (row-major-aref array i))))))

(defparameter *array* (parse *test*))

(defun neighbours (valid-direction? &optional min-count done?)
  (lambda (state)
    (destructuring-bind (row col dir count) state
      (loop for new-dir in '(north east south west)
            for new-row = (case new-dir
                            (north (1- row))
                            (south (1+ row))
                            (t row))
            for new-col = (case new-dir
                            (east (1+ col))
                            (west (1- col))
                            (t col))
            for new-count = (if (eq dir new-dir)
                                (1+ count)
                                1)
            when (and (funcall valid-direction? dir count new-dir)
                      (array-in-bounds-p *array* new-row new-col)
                      (or (null min-count)
                          (not (funcall done? (list new-row new-col)))
                          (<= min-count new-count)))
              collect (list new-row new-col new-dir new-count)))))

(defun done? (array)
  (let ((dst (array-dimensions array)))
    (lambda (x)
      (loop for a in x
            for b in dst
            repeat 2
            always (eql a (1- b))))))

(defun valid-direction? (from count dir)
  (cond ((null from) t)
        ((eq from dir) (< count 3))
        (t (not (eq dir (case from (north 'south) (east 'west) (south 'north) (west 'east)))))))

(defun distance (src dst)
  (declare (ignore src))
  (aref *array* (first dst) (second dst)))

(defun least-heat-loss (array neighbours)
  (let* ((*array* array)
         (n (dijkstra:search* (list 0 0 nil 0) neighbours
                              :distancef #'distance
                              :donep (done? array))))
    (loop for x = n then (dijkstra:previous x)
          while x
          for (row col dir count) = (dijkstra:item x)
          when dir do (setf (aref *array* row col) (case dir
                                                     (north #\^)
                                                     (east #\>)
                                                     (south #\v)
                                                     (west #\<))))
    (aoc:print-array *array*)
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

;; 2023 17 2: '1080'
;; That's not the right answer; your answer is too low.
;; Please wait one minute before trying again.
;; (Cached until 2023-12-17 14:04:45)

(defparameter *test2*
"111111111111
999999999991
999999999991
999999999991
999999999991")
