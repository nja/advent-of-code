;;;; day19.lisp

(in-package :aoc2024.day19)

(defun parse (input)
  (let ((sections (aoc:sections input)))
    (list (ppcre:split ", " (first sections))
          (aoc:lines (second sections)))))

(defparameter *memo* nil)

(defun possible-designs (towels design)
  (multiple-value-bind (value present) (gethash design *memo*)
    (if present
        value
        (setf (gethash design *memo*)
              (if (zerop (length design))
                  1
                  (loop for towel in towels
                        sum (or (and (str:starts-with? towel design)
                                     (possible-designs towels (subseq design (length towel))))
                                0)))))))

(defun part1 (input)
  (let ((*memo* (make-hash-table :test 'equal)))
    (destructuring-bind (towels designs) (parse input)
      (count-if #'plusp (mapcar (a:curry #'possible-designs towels) designs)))))

(defun part2 (input)
  (let ((*memo* (make-hash-table :test 'equal)))
    (destructuring-bind (towels designs) (parse input)
      (reduce #'+ (mapcar (a:curry #'possible-designs towels) designs)))))
