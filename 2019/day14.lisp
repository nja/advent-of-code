;;;; day14.lisp

(in-package :aoc2019.day14)

(defparameter *test*
"10 ORE => 10 A
1 ORE => 1 B
7 A, 1 B => 1 C
7 A, 1 C => 1 D
7 A, 1 D => 1 E
7 A, 1 E => 1 FUEL")

(defun parse (input)
  (flet ((read-plist (s)
           (reverse (read-from-string (format nil "(~a)" (aoc:tr "," "  " s))))))
    (mapcar (lambda (line)
              (mapcar #'read-plist (str:split " => " line)))
            (aoc:lines input))))

(defun costs-lookup (costs)
  (loop with hash = (make-hash-table)
        for entry in costs
        for key = (caadr entry)
        do (setf (gethash key hash) entry)
        finally (return hash)))

(defun add (bag stuff)
  (cond ((null stuff) bag)
        (t (incf (gethash (first stuff) bag 0) (second stuff))
           (add bag (cddr stuff)))))

(defun take (bag stuff)
  (cond ((null stuff) bag)
        (t (assert (<= 0 (decf (gethash (first stuff) bag 0) (second stuff))))
           (take bag (cddr stuff)))))

(defun have? (bag stuff)
  (or (null stuff)
      (and (<= (second stuff) (gethash (first stuff) bag 0))
           (have? bag (cddr stuff))
           bag)))

(defun cost-of (lookup what)
  (first (gethash what lookup)))

(defun amount-of (lookup what)
  (second (gethash what lookup)))

(defun add-unstocked (stock needs stuff)
  (loop for (mat amount) on stuff by #'cddr
        unless (have? stock (list mat amount))
          do (add needs (list mat amount))))

(defclass state ()
  ((stock :reader stock :initform (make-hash-table))
   (needs :reader needs :initform (add (make-hash-table) '(fuel 1)))
   (costs :accessor costs :initarg :costs)
   (mats :accessor mats :initarg :mats)
   (spent :accessor spent :initform 0)))

(defun make-state (costs)
  (make-instance 'state
                 :costs costs
                 :mats (a:hash-table-keys costs)))

(defun act (state)
  (with-slots (stock needs mats costs spent) state
    (dolist (mat mats)
      (when (gethash mat needs)
        (let ((cost (cost-of costs mat)))
          (cond ((have? stock cost)
                 (take stock cost)
                 (add stock(amount-of costs mat))
                 (remhash mat needs))
                (t
                 (add-unstocked stock needs cost))))))
    (incf (gethash 'ore stock 0) (gethash 'ore needs 0))
    (incf spent (gethash 'ore needs))
    (setf (gethash 'ore needs) 0))
  state)