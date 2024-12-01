;;;; day08.lisp

(in-package #:aoc2022.day08)

(defun grid (input)
  (loop with cols = (reduce #'max (aoc:lines input) :key #'length)
        with rows = (length (aoc:lines input))
        with array = (make-array (list rows cols))
        for i from 0
        for d across (remove nil (map 'vector #'digit-char-p input))
        do (setf (row-major-aref array i) d)
        finally (return array)))

(defun visible? (grid row col)
  (let ((max (aref grid row col)))
    (flet ((shorter? (rd cd)
             (loop for r = (+ row rd) then (+ r rd)
                   for c = (+ col cd) then (+ c cd)
                   while (array-in-bounds-p grid r c)
                   always (< (aref grid r c) max))))
      (or (shorter? -1 0) (shorter? 1 0) (shorter? 0 -1) (shorter? 0 1)))))

(defun map-grid (grid f)
  (flet ((subscripts (dimension)
           (loop for i below (array-dimension grid dimension)
                 collect i)))
    (a:map-product (a:curry f grid) (subscripts 0) (subscripts 1))))

(defun part1 (input)
  (count t (map-grid (grid input) #'visible?)))

(defun score (grid row col)
  (let ((max (aref grid row col)))
    (flet ((trees (rd cd)
             (loop for r = (+ row rd) then (+ r rd)
                   for c = (+ col cd) then (+ c cd)
                   for trees from 0
                   while (array-in-bounds-p grid r c)
                   unless (< (aref grid r c) max)
                     do (return (1+ trees))
                   finally (return trees))))
      (* (trees -1 0) (trees 1 0) (trees 0 -1) (trees 0 1)))))

(defun part2 (input)
  (reduce #'max (map-grid (grid input) #'score)))
