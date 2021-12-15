;;;; day15.lisp

(in-package #:aoc2021.day15)

(defparameter *queue* nil)
(defparameter *array* nil)

(defclass node ()
  ((row :reader row :initarg :row)
   (col :reader col :initarg :col)
   (tentative-distance :reader tentative-distance :initform nil)
   (cost :reader cost :initarg :cost)
   (qnode :accessor qnode)))

(defmethod (setf tentative-distance) (new-value (node node))
  (with-slots ((value tentative-distance) qnode) node
    (cond ((null value)
           (prog1 (setf value new-value)
             (setf qnode (nth-value 1 (q:qpush *queue* node)))))
          ((< new-value value)
           (prog1 (setf value new-value)
             (q:queue-change *queue* qnode node)))
          (t value))))

(defun to-array (input)
  (let* ((lines (aoc:lines input))
         (rows (length lines))
         (cols (length (first lines)))
         (array (make-array (list rows cols))))
    (loop for n in (remove nil (map 'list #'digit-char-p input))
          for i from 0
          for (row col) = (multiple-value-list (truncate i cols))
          do (setf (row-major-aref array i)
                   (make-instance 'node :cost n :row row :col col)))
    (setf (tentative-distance (aref array 0 0)) 0)
    array))

(defun saferef (array y x)
  (when (array-in-bounds-p array y x)
    (aref array y x)))

(defun grow-array (small)
  (loop with large = (make-array (mapcar (a:curry #'* 5) (array-dimensions small)))
        for row below (array-dimension large 0) do
          (loop for col below (array-dimension large 1)
                for (r+ sr) = (multiple-value-list
                               (truncate row (array-dimension small 0)))
                for (c+ sc) = (multiple-value-list
                               (truncate col (array-dimension small 1)))
                do (setf (aref large row col)
                         (make-instance 'node :cost (wrap (+ r+ c+ (cost (aref small sr sc))))
                          :row row :col col)))
        finally (return large)))

(defun wrap (x)
  (if (< x 10)
      x
      (wrap (1+ (mod x 10)))))

(defun compare-nodes (a b)
  (cond ((null (tentative-distance b)) (not (null (tentative-distance a))))
        ((null (tentative-distance a)) nil)
        (t (< (tentative-distance a) (tentative-distance b)))))

(defun next-node ()
  (q:qpop *queue*))

(defun target-node ()
  (aref *array*
        (1- (array-dimension *array* 0))
        (1- (array-dimension *array* 1))))

(defun consider (node)
  (dolist (neighbour (neighbours node))
    (setf (tentative-distance neighbour)
          (+ (tentative-distance node) (cost neighbour)))))

(defun neighbours (node)
  (loop for (r+ c+) in '((-1 0) (0 -1) (1 0) (0 1))
        for n = (saferef *array* (+ r+ (row node)) (+ c+ (col node)))
        when n collect it))

(defun find-path-to (target)
  (loop for node = (next-node)
        until (eq node target)
        do (consider node)
        finally (return target)))

(defun part1 (input)
  (let* ((*queue* (q:make-queue :priority-queue :compare #'compare-nodes))
         (*array* (to-array input)))
    (tentative-distance (find-path-to (target-node)))))

(defun part2 (input)
  (let* ((*queue* (q:make-queue :priority-queue :compare #'compare-nodes))
         (*array* (grow-array (to-array input))))
    (tentative-distance (find-path-to (target-node)))))
