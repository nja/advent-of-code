;;;; day08.lisp

(in-package :aoc2019.day08)

(defparameter *rows* 6)
(defparameter *cols* 25)

(defun layers (digits)
  (loop with length = (* *rows* *cols*)
        for start = 0 then end
        for end from length by length
        while (<= start end (length digits))
        collect (subseq digits start end)))

(defun fewest (key list)
  (reduce (lambda (a b)
            (if (< (funcall key a) (funcall key b))
                a
                b))
          list))

(defun part1 (input)
  (let ((layer (fewest (a:curry #'count #\0) (layers input))))
    (* (count #\1 layer) (count #\2 layer))))

(defun pixel (char)
  (getf '(#\0 #\. #\1 #\# #\2 #\Space) char))

(defun decode (layers)
  (let ((rows (loop repeat *rows* collect (make-string *cols* :initial-element #\Space))))
    (dolist (layer layers rows)
      (loop for row in rows
            for offset by *cols* do
              (loop for i from 0 below (length row)
                    when (eql #\Space (aref row i))
                      do (setf (aref row i) (pixel (aref layer (+ offset i)))))))))

(defun part2 (input)
  (decode (layers input)))
