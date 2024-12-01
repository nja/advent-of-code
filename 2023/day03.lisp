;;;; day03.lisp

(in-package :aoc2023.day03)

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
  (reduce #'+ (numbers (copy-part-numbers (aoc:to-array input)))))

(defun adjacent-numbers (array row col)
  (let ((tmp (make-array (array-dimensions array) :initial-element #\.)))
    (loop for (r c) in (adjacents array row col)
          do (copy-digits array tmp r c))
    (numbers tmp)))

(defun gear-ratios (array)
  (loop for row below (array-dimension array 0) append
    (loop for col below (array-dimension array 1)
          for r = (and (char= #\* (aref array row col))
                       (adjacent-numbers array row col))
          when (and r (= 2 (length r)))
            collect r)))

(defun part2 (input)
  (reduce #'+ (mapcar (a:curry #'reduce #'*) (gear-ratios (aoc:to-array input)))))

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
