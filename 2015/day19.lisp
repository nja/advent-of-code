;;;; day19.lisp

(in-package :aoc2015.day19)

(defun parse (input)
  (values (mapcar (a:curry #'str:split " => ")
                  (aoc:lines (first (aoc:sections input))))
          (first (aoc:lines (second (aoc:sections input))))))

(defun count-matches (regex string)
  (/ (length (ppcre:all-matches regex string)) 2))

(defun distinct-molecules (replacements molecule)
  (loop with distinct = (make-hash-table :test 'equal)
        for (pattern replacement) in replacements
        do (ppcre:do-matches (start end pattern molecule)
             (let ((replaced (concatenate 'string
                                          (subseq molecule 0 start)
                                          replacement
                                          (subseq molecule end))))
               (incf (gethash replaced distinct 0))))
        finally (return (a:hash-table-keys distinct))))

(defun part1 (input)
  (length (multiple-value-call #'distinct-molecules (parse input))))

(defun steps (replacements start target)
  (labels ((rec (molecule steps next)
             (if (equal molecule target)
                 steps
                 (labels ((try-next (options)
                            (if (null options)
                                (funcall next)
                                (rec (car options) (1+ steps)
                                     (lambda () (try-next (cdr options)))))))
                   (try-next (sort (distinct-molecules replacements molecule)
                                   #'< :key #'length))))))
    (rec start 0 (lambda ()))))

(defun part2 (input)
  (multiple-value-bind (replacements molecule) (parse input)
    (steps (mapcar #'reverse replacements) molecule "e")))
