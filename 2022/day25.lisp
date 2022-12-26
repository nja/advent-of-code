;;;; day25.lisp

(in-package :aoc2022.day25)

(defun read-snafu (string)
  (labels ((value (digit)
             (- (position digit "=-012") 2))
           (rec (digits sum)
             (if (null digits)
                 sum
                 (rec (cdr digits) (+ (value (car digits)) (* sum 5))))))
    (rec (coerce string 'list) 0)))

(defun snafu-digits (n)
  (labels ((digit (n)
             (elt '(0 1 2 = -) (mod n 5)))
           (rec (n digits)
             (if (zerop n)
                 (or digits '(0))
                 (rec (if (integerp (digit n))
                          (truncate n 5)
                          (1+ (truncate n 5)))
                      (cons (digit n) digits)))))
    (rec n nil)))

(defun to-string (digits)
  (format nil "~{~a~}" digits))

(defun part1 (input)
  (let ((sum (reduce #'+ (mapcar #'read-snafu (aoc:lines input)))))
    (values (to-string (snafu-digits sum))
            sum)))
