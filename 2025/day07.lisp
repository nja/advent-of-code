;;;; day07.lisp

(in-package :aoc2025.day07)

(defun parse (input)
  (aoc:to-array (aoc:tr "S" "|" input)))

(defun beam-row (array row)
  (loop for col below (array-dimension array 1)
        for beam? = (char= (aref array row col) #\|)
        for split? = (char= (aref array (1+ row) col) #\^)
        count (and beam? split? )
        when beam?
          do (if split?
                 (setf (aref array (1+ row) (1- col)) #\|
                       (aref array (1+ row) (1+ col)) #\|)
                 (setf (aref array (1+ row) col) #\|))))

(defun beam (array)
  (loop for row from 0 below (1- (array-dimension array 0))
        sum (beam-row array row)))

(defun part1 (input)
  (beam (parse input)))

(defun quantum-row (array row timelines)
  (loop with next = (make-array (length timelines))
        for i from 0
        for n across timelines
        do (if (eql #\^ (aref array (1+ row) i))
               (progn (incf (aref next (1- i)) n)
                      (incf (aref next (1+ i)) n))
               (incf (aref next i) n))
        finally (return next)))

(defun quantum-beams (array)
  (loop for row below (1- (array-dimension array 0))
        for timelines = (quantum-row array row (or timelines (timelines array row)))
        collect timelines))

(defun timelines (array row)
  (coerce (loop for i below (array-dimension array 1)
                collect (if (eql (aref array row i) #\|) 1 0))
          'vector))

(defun part2 (input)
  (reduce #'+ (a:lastcar (quantum-beams (parse input)))))
