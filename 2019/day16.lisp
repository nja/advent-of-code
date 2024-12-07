;;;; day16.lisp

(in-package :aoc2019.day16)

(defun parse (input)
  (remove nil (map 'vector #'digit-char-p input)))

(defun pattern (base n)
  (let ((i 0) (c 0))
    (flet ((generator ()
             (prog1 (aref base i)
               (when (= (incf c) n)
                 (incf i)
                 (setf c 0))
               (when (= i (length base))
                 (setf i 0)))))
      (generator)
      #'generator)))

(defun phase1 (input)
  (loop with output = (make-array (length input))
        for i below (length input)
        do (setf (aref output i) (element input i))
        finally (return output)))

(defun element (input i)
  (abs (rem (loop with p = (pattern #(0 1 0 -1) (1+ i))
                  for x across input
                  for v = (* x (funcall p))
                  sum v)
            10)))

(defun phases (input n)
  (dotimes (i n input)
    (setf input (phase1 input))))

(defun unparse (offset n seq)
  (map 'string #'digit-char (subseq seq offset (+ offset n))))

(defun part1 (input)
  (unparse 0 8 (phases (parse input) 100)))

(defun repeat (vector n)
  (loop with output = (make-array (* n (length vector)) :fill-pointer 0)
        repeat n
        do (loop for x across vector
                 do (vector-push x output))
        finally (return output)))

(defun message-offset (vector)
  (parse-integer (unparse 0 7 vector)))

(defun backfill (vector offset)
  (loop for i from (1- (length vector)) downto offset
        sum (aref vector i) into sum
        do (setf (aref vector i) (rem sum 10))))

(defun part2 (input)
  (let* ((vector (repeat (parse input) 10000))
         (offset (message-offset vector)))
    (dotimes (n 100 (unparse offset 8 vector))
      (backfill vector offset))))
