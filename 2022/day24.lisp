;;;; day24.lisp

(in-package :aoc2022.day24)

(defun to-array (input)
  (loop with rows = (- (length (aoc:lines input)) 2)
        with cols = (- (length (first (aoc:lines input))) 2)
        with array = (make-array (list rows cols))
        for line in (cdr (aoc:lines input))
        for row below rows
        do (loop for col below cols
                 for c across (subseq line 1 (1+ cols))
                 do (setf (aref array row col) c))
        finally (return array)))

(defparameter *array* nil)

(defun empty? (row col i)
  (flet ((ref (r c)
           (aref *array*
                 (mod (+ row (* i r)) (array-dimension *array* 0))
                 (mod (+ col (* i c)) (array-dimension *array* 1)))))
    (or (start? row col i)
        (goal? row col i)
        (and (array-in-bounds-p *array* row col)
             (not (or (eql #\> (ref 0 -1))
                      (eql #\< (ref 0 1))
                      (eql #\^ (ref 1 0))
                      (eql #\v (ref -1 0))))))))

(defun neighbours (state)
  (destructuring-bind (row col minute) state
    (let ((m (1+ minute)))
      (remove nil (mapcar (lambda (r c)
                            (when (empty? (+ row r) (+ col c) m)
                              (list (+ row r) (+ col c) m)))
                          '(1 -1 0  0 0)
                          '(0  0 1 -1 0))))))

(defun goal? (row col minute)
  (declare (ignore minute))
  (and (eql row (array-dimension *array* 0))
       (eql col (1- (array-dimension *array* 1)))))

(defun start? (row col minute)
  (declare (ignore minute))
  (and (eql row -1)
       (eql col 0)))

(defun path (from to?)
  (d:item (d:search* from #'neighbours :donep (a:curry #'apply to?))))

(defun part1 (input)
  (let ((*array* (to-array input)))
    (third (path '(-1 0 0) #'goal?))))

(defun part2 (input)
  (let* ((*array* (to-array input))
         (goal (path '(-1 0 0) #'goal?))
         (start (path goal #'start?)))
    (third (path start #'goal?))))
