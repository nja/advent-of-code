;;;; day03.lisp

(in-package :aoc2023.day03)

(defun to-array (input)
  (loop with lines = (aoc:lines input)
        with rows = (length lines)
        with cols = (length (first lines))
        with array = (make-array (list rows cols))
        for c across (remove #\Newline input)
        for i from 0
        do (setf (row-major-aref array i) c)
        finally (return array)))

(defun copy-digits (src dst row col)
  (flet ((digit? (col)
           (and (array-in-bounds-p src row col)
                (digit-char-p (aref src row col)))))
    (when (digit? col)
      (loop while (digit? (1- col))
            do (decf col))
      (loop while (digit? col)
            do (setf (aref dst row col)
                     (aref src row col))
               (incf col)))))

(defun adjacents (array row col)
  (loop for (r c) in '((-1 -1) (-1 0) (-1 1)
                       ( 0 -1)        ( 0 1)
                       ( 1 -1) ( 1 0) ( 1 1))
        for subscripts = (list (+ r row) (+ c col))
        when (apply #'array-in-bounds-p array subscripts)
          collect subscripts))

(defun symbol? (c)
  (not (or (digit-char-p c)
           (char= #\. c))))

(defun copy-part-numbers (src)
  (let ((dst (make-array (array-dimensions src) :initial-element #\.)))
    (loop for row below (array-dimension src 0) do
      (loop for col below (array-dimension src 1)
            when (symbol? (aref src row col))
              do (loop for (r c) in (adjacents src row col)
                       do (copy-digits src dst r c))))
    dst))

(defun numbers (array)
  (read-from-string
   (with-output-to-string (s)
     (format s "(")
     (loop for row below (array-dimension array 0) do
       (loop for col below (array-dimension array 1)
             for c = (digit-char-p (aref array row col))
             do (format s "~a" (or c #\Space)))
       (format s " "))
     (format s ")"))))

(defun part1 (input)
  (reduce #'+ (numbers (copy-part-numbers (to-array input)))))

(defparameter *test*
  "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..")



(defun xcopy (src)
  (let ((dst (make-array (array-dimensions src) :initial-element #\.)))
    (loop for row from 20 below 23 do
      (loop for col below 10
            when (symbol? (aref src row col))
              do (loop for (r c) in (adjacents src row col)
                       do (setf (aref dst r c) (aref src row col)))))
    dst))
