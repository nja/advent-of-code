;;;; day10.lisp

(in-package :aoc2024.day10)

(defun parse (input)
  (let ((map (aoc:to-array input)))
    (loop for i below (array-total-size map)
          do (setf (row-major-aref map i)
                   (digit-char-p (row-major-aref map i))))
    map))

(defun trailheads (map)
  (loop with trailheads
        for row below (array-dimension map 0)
        do (loop for col below (array-dimension map 1)
                 when (eql 0 (aref map row col))
                   do (push (list row col) trailheads))
        finally (return (nreverse trailheads))))

(defun trail-score (map head)
  (count 9 (mapcar (lambda (node) (height map (dijkstra:item node)))
                   (dijkstra:search* head (neighbours map)))))

(defun neighbours (map &optional (pos #'identity) (neighbour (lambda (a b) (declare (ignore b)) a)))
  (lambda (node)
    (loop with p = (funcall pos node)
          with h = (height map p)
          for np in (mapcar (a:curry #'add p) '((1 0) (-1 0) (0 1) (0 -1)))
          for nh = (height map np)
          for s = (and nh (- nh h))
          when (eql s 1)
            collect (funcall neighbour np node))))

(defun height (map p)
  (and (apply #'array-in-bounds-p map p)
       (apply #'aref map p)))

(defun add (a b) (mapcar #'+ a b))

(defun part1 (input)
  (let ((map (parse input)))
    (reduce #'+ (mapcar (a:curry #'trail-score map) (trailheads map)))))

(defun distinct-trails (map head)
  (count 9 (mapcar (lambda (node) (height map (car (dijkstra:item node))))
                   (dijkstra:search* (list head) (neighbours map #'car #'cons)))))

(defun part2 (input)
  (let ((map (parse input)))
    (reduce #'+ (mapcar (a:curry #'distinct-trails map) (trailheads map)))))
