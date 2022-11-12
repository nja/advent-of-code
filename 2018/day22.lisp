;;;; day22.lisp

(in-package #:aoc2018.day22)

(defun clean (line)
  (mapcar #'parse-integer (remove-if (a:curry #'string= "") (ppcre:split "[^0-9]+" line))))

(defvar *erosion-levels*)
(defvar *depth*)
(defvar *target-x*)
(defvar *target-y*)

(defun geological-index (x y)
  (cond ((= 0 x y) 0)
        ((target? x y)
         0)
        ((zerop x) (* y 48271))
        ((zerop y) (* x 16807))
        (t (* (erosion-level (1- x) y)
              (erosion-level x (1- y))))))

(defun erosion-level (x y)
  (or (aref *erosion-levels* y x)
      (setf (aref *erosion-levels* y x)
            (mod (+ *depth* (geological-index x y)) 20183))))

(defun region-type (erosion-level)
  (case (mod erosion-level 3)
    (0 #\.)
    (1 #\=)
    (2 #\|)))

(defun target? (x y)
  (and (= *target-x* x) (= *target-y* y)))

(defun print-array (array)
  (aoc:print-indexed-lines
   (loop for row below (array-dimension array 0)
         collect (with-output-to-string (s)
                   (loop for col below (array-dimension array 1)
                         do (princ (cond ((= 0 row col) #\M)
                                         ((target? col row) #\T)
                                         (t (region-type (erosion-level col row))))
                                   s))))))

(defun part1 (input)
  (destructuring-bind (*depth* *target-x* *target-y*) (clean input)
    (let ((*erosion-levels* (make-array (list (1+ *target-y*) (1+ *target-x*)) :initial-element nil)))
      (loop for row below (array-dimension *erosion-levels* 0)
            sum (loop for col below (array-dimension *erosion-levels* 1)
                      sum (mod (erosion-level col row) 3))))))

(defstruct (state (:constructor state (x y tool))) x y tool)

(defun neighbours (state)
  (let (result
        (x (state-x state))
        (y (state-y state))
        (tool (state-tool state)))
    (labels ((collect (x y)
               (when (and (<= 0 x) (<= 0 y) (find tool (local-tools x y)))
                 (push (state x y tool) result))))
      (collect (1- x) y)
      (collect (1+ x) y)
      (collect x (1- y))
      (collect x (1+ y))
      (push (state x y (find tool (local-tools x y) :test-not #'eq)) result))
    result))

(defun local-tools (x y)
  (case (mod (erosion-level y x) 3)
    (0 '(climbing-gear torch))
    (1 '(climbing-gear neither))
    (2 '(torch neither))))

(defun distance (a b)
  (let ((a (d:item a)))
    (if (eq (state-tool a) (state-tool b))
        1
        7)))

(defun search* (from to)
  (d:search* from #'neighbours :distancef #'distance :donep (a:curry #'equalp to)))

(defun part2 (input)
  (destructuring-bind (*depth* *target-x* *target-y*) (clean input)
    (let ((*erosion-levels* (make-array '(1000 1000) :initial-element nil)))
      (d:distance (search* (state 0 0 'torch) (state *target-x* *target-y* 'torch))))))
