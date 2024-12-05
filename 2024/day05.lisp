;;;; day05.lisp

(in-package :aoc2024.day05)

(defun rules (section)
  (let ((rules (make-hash-table)))
    (flet ((add (a b) (push b (gethash a rules))))
      (mapc (a:curry #'apply #'add)
            (mapcar #'aoc:read-integers (aoc:lines section))))
    rules))

(defun updates (section)
  (mapcar #'aoc:read-integers (aoc:lines section)))

(defun parse (input)
  (aoc:map-sections input #'rules #'updates))

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
  (destructuring-bind (rules updates) (parse input)
    (let ((corrected (mapcar (a:curry #'order rules)
                             (incorrectly-ordered rules updates))))
      (reduce #'+ (middle-pages corrected)))))
