;;;; day24.lisp

(in-package #:aoc2016.day24)

(defparameter *array* nil)

(defun to-array (input)
  (let ((rows (length (aoc:lines input)))
        (cols (reduce #'max (aoc:lines input) :key #'length)))
    (loop with array = (make-array (list rows cols))
          for c across (remove #\Newline input)
          for i from 0
          do (setf (row-major-aref array i) c)
          finally (return array))))

(defun find-index (item)
  (loop for i below (array-total-size *array*)
        for x = (row-major-aref *array* i)
        when (eql x item)
          do (return i)))

(defun index (row col)
  (array-row-major-index *array* row col))

(defun subscripts (index)
  (truncate index (array-dimension *array* 1)))

(defun neighbours (index)
  (let (result)
    (flet ((collect (row col)
             (when (and (array-in-bounds-p *array* row col)
                        (not (eql #\# (aref *array* row col))))
               (push (index row col) result))))
      (multiple-value-bind (row col) (subscripts index)
        (collect (1+ row) col)
        (collect (1- row) col)
        (collect row (1+ col))
        (collect row (1- col))))
    result))

(defun ref (index)
  (row-major-aref *array* index))

(defun distances (nodes)
  (when (plusp (length nodes))
    (append (remove nil (mapcar (lambda (node)
                                  (a:when-let (to (find (ref (dijkstra:item node)) nodes :start 1))
                                    (cons (key (aref nodes 0) to) (dijkstra:distance node))))
                                (dijkstra:search* (find-index (aref nodes 0)) #'neighbours)))
            (distances (subseq nodes 1)))))

(defun key (a b)
  (if (char< a b) (cons a b) (cons b a)))

(defparameter *distances* nil)

(defun distance (from to)
  (cdr (assoc (key from to) *distances* :test #'equal)))

(defparameter *return* nil)

(defun travel (from destinations)
  (if (zerop (length destinations))
      (if *return*
          (distance from #\0)
          0)
      (reduce #'min (map 'list (lambda (to)
                                 (+ (distance from to)
                                    (travel to (remove to destinations))))
                         destinations))))

(defun part1 (input)
  (let* ((destinations (sort (remove-if-not #'digit-char-p input) #'char<))
         (*array* (to-array input))
         (*distances* (distances destinations)))
    (travel (aref destinations 0) (subseq destinations 1))))

(defun part2 (input)
  (let ((*return* t))
    (part1 input)))
