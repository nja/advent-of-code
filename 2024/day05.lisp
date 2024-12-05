;;;; day05.lisp

(in-package :aoc2024.day05)

(defun parse-rules (input)
  (let ((rules (make-hash-table)))
    (flet ((add (a b) (push b (gethash a rules))))
      (map nil (a:curry #'apply #'add) (mapcar #'aoc:read-integers (aoc:lines (first (aoc:sections input))))))
    rules))

(defun parse-updates (input)
  (mapcar (lambda (line)
            (read-from-string (format nil "(~a)" (aoc:tr "," " " line))))
          (aoc:lines (second (aoc:sections input)))))

(defun parse (input)
  (list (parse-rules input) (parse-updates input)))

(defun right-order? (rules updates)
  (if (null updates)
      t
      (and (not (some (lambda (x) (is-earlier? rules x (first updates)))
                      (rest updates)))
           (right-order? rules (rest updates)))))

(defun is-earlier? (rules x y)
  (member y (gethash x rules)))

(defun middle-pages (updates)
  (mapcar (lambda (x) (nth (truncate (length x) 2) x)) updates))

(defun part1 (input)
  (destructuring-bind (rules updates) (parse input)
    (let ((correctly-ordered (remove-if-not (a:curry #'right-order? rules) updates)))
      (reduce #'+ (middle-pages correctly-ordered)))))

(defun incorrectly-ordered (rules updates)
  (remove-if (a:curry #'right-order? rules) updates))

(defun order (rules update)
  (sort update (a:curry #'is-earlier? rules)))

(defun part2 (input)
  (let* ((rules (parse-rules input))
         (updates (parse-updates input))
         (corrected (mapcar (a:curry #'order rules) (incorrectly-ordered rules updates))))
    (reduce #'+ (middle-pages corrected))))
