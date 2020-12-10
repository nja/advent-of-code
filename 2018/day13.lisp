;;;; day13.lisp

(in-package #:aoc2018.day13)

(defparameter *test* "/->-\
|   |  /----\
| /-+--+-\  |
| | |  | v  |
\-+-/  \-+--/
  \------/   ")

(defvar *tracks* nil)

(defun to-array (lines)
  (loop with rows = (length lines)
        with cols = (reduce #'max (mapcar #'length lines))
        with array = (make-array (list rows cols) :initial-element #\Space)
        for y from 0
        for line in lines
        do (loop for x from 0
                 for ch across line
                 do (setf (aref array y x) ch))
        finally (return array)))

(defun digits (n) (length (format nil "~d" n)))
(defun getfmt (n) (format nil "~~~d,d" (digits n)))
(defun getspc (n) (format nil "~{~a~}" (make-list (digits n) :initial-element #\Space)))

(defun top-row (rows cols i)
  (format t "~&~a" (getspc rows))
  (loop with fmt = (getfmt cols)
        for n below cols
        for s = (format nil fmt n)
        for c = (aref s i)
        do (princ c)))

(defun print-array (array)
  (destructuring-bind (rows cols) (array-dimensions array)
    (loop for i from 0 below (digits cols) do (top-row rows cols i))
    (loop with fmt = (getfmt rows)
          for y below rows
          do (format t "~&")
             (format t fmt y)
             (loop for x below cols do (princ (aref array y x))))))

(defparameter *dirs* "^>v<")
(defun cart-char? (ch) (find ch *dirs*))
(defun cart (ch y x) (list ch y x 'left))
(defun dir (cart) (first cart))
(defun x (cart) (third cart))
(defun y (cart) (second cart))
(defun right (ch) (aref *dirs* (mod (1+ (position ch *dirs*)) 4)))
(defun left (ch) (aref *dirs* (mod (1- (position ch *dirs*)) 4)))
(defun under (cart) (aref *tracks* (y cart) (x cart)))
(defun corner (cart)
  (case (dir cart)
    ((#\^) )))

(defun collect-carts (array)
  (loop for y below (array-dimension array 0)
        nconc (loop for x below (array-dimension array 1)
                    for ch = (aref array y x)
                    when (cart-char? ch) collect (cart ch y x))))

