;;;; day18.lisp

(in-package #:aoc2021.day18)

(defun parse-input (input)
  (mapcar #'parse (aoc:lines input)))

(defun parse (line)
  (box-numbers (read-from-string (str:replace-all "," " . " (aoc:tr "[]" "()" line)))))

(defun box-numbers (x)
  (cond ((null x) nil)
        ((integerp x) (list x))
        (t (cons (box-numbers (car x)) (box-numbers (cdr x))))))

(defun pair (left right) (cons left right))
(defun left (pair) (car pair))
(defun right (pair) (cdr pair))
(defun pair? (x) (when (not (null (cdr x))) x))
(defun number* (x) (list x))
(defun number? (x) (integerp (car x)))
(defun value (number) (car number))

(defun left-pair (x) (and (pair? x) (pair? (left x))))
(defun right-pair (x) (and (pair? x) (pair? (right x))))

(defun flatten (x)
  (cond ((number? x) (list x))
        ((pair? x)
         (nconc (flatten (left x)) (flatten (right x))))))

(defun neighbours (x pair)
  (loop for (a b c) on (cons nil (flatten pair))
        when (eq x b) return (list a c)
          finally (return (list nil nil))))

(defun reduce* (pair)
  (or (a:when-let ((explodes (explodes? pair)))
        (explode explodes pair))
      (a:when-let ((splits (splits? pair)))
        (split splits pair))
      pair))

(defun fixed-point (f x)
  (let ((next (funcall f x)))
    (if (equal x next)
        x
        (fixed-point f next))))

(defun explodes? (x &optional (depth 0))
  (cond ((number? x) nil)
        ((= depth 3) (or (left-pair x) (right-pair x)))
        (t (or (explodes? (left x) (1+ depth))
               (explodes? (right x) (1+ depth))))))

(defun explode (x pair)
  (let* ((zero (number* 0))
         (pair (subst zero x pair :test #'eq)))
    (destructuring-bind (left right) (neighbours zero pair)
      (when left (incf (car left) (value (left x))))
      (when right (incf (car right) (value (right x))))
      pair)))

(defun splits? (x)
  (or (and (number? x) (<= 10 (value x)) x)
      (and (pair? x)
           (or (splits? (left x))
               (splits? (right x))))))

(defun split (x pair)
  (subst (pair (number* (floor (value x) 2))
               (number* (ceiling (value x) 2)))
         x
         pair))

(defun magnitude (x)
  (if (number? x)
      (value x)
      (+ (* 3 (magnitude (left x)))
         (* 2 (magnitude (right x))))))

(defun add (a b)
  (fixed-point #'reduce* (pair a b)))

(defun part1 (input)
  (magnitude (reduce #'add (parse-input input))))

(defun pairs (list)
  (remove nil (a:map-product (lambda (a b)
                               (unless (eq a b)
                                 (copy-tree (list a b))))
                             list list)))

(defun part2 (input)
  (reduce #'max (mapcar (a:compose #'magnitude (a:curry #'apply #'add))
                        (pairs (parse-input input)))))
