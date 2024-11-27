;;;; day14.lisp

(in-package :aoc2019.day14)

(defparameter *test*
"10 ORE => 10 A
1 ORE => 1 B
7 A, 1 B => 1 C
7 A, 1 C => 1 D
7 A, 1 D => 1 E
7 A, 1 E => 1 FUEL")

(defparameter *test2*
"9 ORE => 2 A
8 ORE => 3 B
7 ORE => 5 C
3 A, 4 B => 1 AB
5 B, 7 C => 1 BC
4 C, 1 A => 1 CA
2 AB, 3 BC, 4 CA => 1 FUEL")

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

(defun process (state)
  (with-slots (stock needs mats costs) state
    (dolist (mat mats state)
      (when (gethash mat needs)
        (destructuring-bind (cost amount) (gethash mat costs)
          (cond ((have? stock cost)
                 (take stock cost)
                 (add stock amount)
                 (remhash mat needs)
                 (format t "~a => ~a~%" cost amount))
                (t
                 (add-unstocked stock needs cost))))))))

(defun extract (state)
  (with-slots (stock needs spent) state
    (let ((n (gethash 'ore needs 0)))
      (incf (gethash 'ore stock 0) n)
      (incf spent n)
      (setf (gethash 'ore needs) 0)))
  state)

(defun report (state)
  (format t "Stock: ~a~%" (a:hash-table-plist (stock state)))
  (format t "Needs: ~a~%" (a:hash-table-plist (needs state)))
  (format t "Spent: ~d~%" (spent state))
  state)