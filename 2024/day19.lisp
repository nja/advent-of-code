;;;; day19.lisp

(in-package :aoc2024.day19)

(defun parse (input)
  (let ((sections (aoc:sections input)))
    (list (ppcre:split ", " (first sections))
          (aoc:lines (second sections)))))

(defparameter *memo* (make-hash-table :test 'equal))

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

(defun design-counts (towels designs)
  (mapcar (a:curry #'possible-designs towels) designs))

(defun part1 (input)
  (count-if #'plusp (apply #'design-counts (parse input))))

(defun part2 (input)
  (reduce #'+ (apply #'design-counts (parse input))))
