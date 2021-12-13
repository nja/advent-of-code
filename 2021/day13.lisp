;;;; day13.lisp

(in-package #:aoc2021.day13)

(defun to-array (section)
  (let ((pairs (pairs section)))
    (destructuring-bind (max-x max-y) (pairs-max pairs)
      (let ((array (make-array (list (1+ max-y) (1+ max-x))
                               :initial-element #\.)))
        (loop for (x y) in pairs
              do (setf (aref array y x) #\#)
              finally (return array))))))

(defun pairs (section)
  (mapcar #'pair (aoc:lines section)))

(defun pair (line)
  (mapcar #'parse-integer (str:split #\, line)))

(defun pairs-max (pairs)
  (reduce (a:curry #'mapcar #'max) pairs))

(defun parse-folds (section)
  (mapcar #'parse-fold (aoc:lines section)))

(defun parse-fold (line)
  (ppcre:register-groups-bind (((aoc:symbols '(x y)) dim) (#'parse-integer pos))
      ("fold along (x|y)=(\\d+)" line)
    (list (case dim (x 1) (y 0)) pos)))

(defun folded-dimensions (dimensions fold-dimension)
  (destructuring-bind (rows cols) dimensions
   (case fold-dimension
     (0 (list (truncate rows 2) cols))
     (1 (list rows (truncate cols 2))))))

(defun folded-dimension (array dimension)
  (truncate (array-dimension array dimension) 2))

(defun fold-along (array dimension)
  (let ((folded-array (make-array (folded-dimensions (array-dimensions array) dimension))))
    (loop for row below (array-dimension folded-array 0) do
      (loop for col below (array-dimension folded-array 1) do
        (setf (aref folded-array row col) (aref array row col))))
    (loop for row below (array-dimension folded-array 0) do
      (loop for col below (array-dimension folded-array 1)
            for (old-row old-col)
              = (translate (array-dimensions array) dimension row col)
            when (char= #\# (aref array old-row old-col))
              do (setf (aref folded-array row col) #\#)))
    folded-array))

(defun translate (old-dimensions dimension row col)
  (destructuring-bind (old-rows old-cols) old-dimensions
    (case dimension
      (0 (list (- old-rows row 1) col))
      (1 (list row (- old-cols col 1))))))

(defun count-dots (array)
  (loop for i below (array-total-size array)
        count (char= #\# (row-major-aref array i))))

(defun do-fold (array fold)
  (destructuring-bind (dimension pos) fold
    (let ((folded-array (fold-along array dimension)))
      (assert (= (array-dimension folded-array dimension) pos))
      folded-array)))

(defun part1 (input)
  (count-dots
   (reduce #'do-fold (subseq (parse-folds (second (aoc:sections input))) 0 1)
           :initial-value (to-array (first (aoc:sections input))))))

(defun part2 (input)
  (with-output-to-string (*standard-output*)
    (aoc:print-array
     (reduce #'do-fold (parse-folds (second (aoc:sections input)))
             :initial-value (to-array (first (aoc:sections input)))))))
