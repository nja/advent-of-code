;;;; day17.lisp

(in-package #:aoc2016.day17)

(defparameter *passcode* "hijkl")

(defun parse (input)
  (string-trim '(#\Newline #\Return) input))

(defun copy-char-codes (string buf &optional (start 0))
  (loop for c across string
        for i from start
        do (setf (aref buf i) (char-code c))
        finally (return i)))

(defun hash (path)
  (let ((buf (make-array (+ (length *passcode*) (length path))
                         :element-type '(unsigned-byte 8))))
    (copy-char-codes *passcode* buf)
    (copy-char-codes path buf (length *passcode*))
    (i:digest-sequence :md5 buf)))

(defun doors (hash)
  (flet ((open? (x) (< #xa x)))
    (values (open? (ldb (byte 4 4) (aref hash 0)))
            (open? (ldb (byte 4 0) (aref hash 0)))
            (open? (ldb (byte 4 4) (aref hash 1)))
            (open? (ldb (byte 4 0) (aref hash 1))))))

(defun pos (path)
  (loop for c across path
        sum (case c (#\L -1) (#\R 1) (t 0)) into x
        sum (case c (#\U -1) (#\D 1) (t 0)) into y
        finally (return (values x y))))

(defun neighbours (path)
  (unless (vault? path)
    (let (result)
      (flet ((take (c) (push (format nil "~a~a" path c) result)))
        (multiple-value-bind (up down left right) (doors (hash path))
          (multiple-value-bind (x y) (pos path)
            (when (and up (< 0 y)) (take "U"))
            (when (and down (< y 3)) (take "D"))
            (when (and left (< 0 x)) (take "L"))
            (when (and right (< x 3)) (take "R")))))
      result)))

(defun vault? (path)
  (multiple-value-bind (x y) (pos path)
    (= x y 3)))

(defun shortest-path (*passcode*)
  (d:item (d:search* "" #'neighbours :donep #'vault?)))

(defun part1 (input)
  (shortest-path (parse input)))

(defun longest-path (*passcode*)
  (reduce #'max (remove-if-not (a:compose #'vault? #'d:item)
                               (nth-value 1 (d:search* "" #'neighbours :pathsp t)))
          :key #'d:distance))

(defun part2 (input)
  (longest-path (parse input)))
