;;;; day11.lisp

(in-package #:aoc2021.day11)

(defun to-array (input)
  (loop with array = (make-array '(10 10))
        for n in (remove nil (map 'list #'digit-char-p input))
        for i from 0
        do (setf (row-major-aref array i) n)
        finally (return array)))

(defun make-flashers () (make-array 100 :fill-pointer 0))

(defun increment (array &optional (flashers (make-flashers)))
  (setf (fill-pointer flashers) 0)
  (loop for i below (array-total-size array)
        when (= 10 (incf (row-major-aref array i)))
          do (vector-push i flashers)
        finally (return flashers)))

(defun make-neighbours () (make-array 8 :fill-pointer 0))

(defun neighbours (i &optional (neighbours (make-neighbours)))
  (setf (fill-pointer neighbours) 0)
  (multiple-value-bind (row col) (truncate i 10)
    (loop for (y x) in '((-1 -1) (-1 0) (-1  1)
                         ( 0 -1)        ( 0  1)
                         ( 1 -1) ( 1 0) ( 1  1))
          for r = (+ row y)
          for c = (+ col x)
          for i = (+ (* r 10) c)
          when (and (<= 0 r 9) (<= 0 c 9))
            do (vector-push i neighbours)
          finally (return neighbours))))

(defun in-bounds? (i)
  (<= 0 i 99))

(defun flash (array flashers &optional (neighbours (make-neighbours)))
  (loop for i from 0
        while (< i (fill-pointer flashers))
        for f = (aref flashers i)
        do (loop for n across (neighbours f neighbours)
                 if (and (in-bounds? n)
                         (= 10 (incf (row-major-aref array n))))
                   do (vector-push n flashers)))
  (loop for f across flashers
        do (setf (row-major-aref array f) 0))
  (length flashers))

(defun steps (array n)
  (loop with flashers = (make-flashers)
        with neighbours = (make-neighbours)
        repeat n
        sum (flash array (increment array flashers) neighbours)))

(defun part1 (input)
  (steps (to-array input) 100))

(defun synced (array)
  (loop with flashers = (make-flashers)
        with neighbours = (make-neighbours)
        for i from 1
        for n = (flash array (increment array flashers) neighbours)
        when (= n (array-total-size array))
          return i))

(defun part2 (input)
  (synced (to-array input)))
