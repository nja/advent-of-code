;;;; day01.lisp

(in-package :aoc2023.day01)

(defun parse-digits (&rest digits)
  (parse-integer
   (with-output-to-string (s)
     (dolist (d digits)
       (format s "~a" d)))))

(defun calibration-value (line)
  (parse-digits
   (find-if #'digit-char-p line)
   (find-if #'digit-char-p line :from-end t)))

(defun part1 (input)
  (reduce #'+ (mapcar #'calibration-value (aoc:lines input))))

(defparameter *replacements*
  (loop for d from 1 upto 10
        collect (list (format nil "~r" d)
                      (format nil "~d" d)
                      d)))

(defun find-digit (line &key from-end)
  (let (best digit (comparison (if from-end #'> #'<)))
    (flet ((f (s d)
             (let ((i (search s line :from-end from-end)))
               (when (and i (or (null best) (funcall comparison i best)))
                 (setf best i digit d)))))
      (dolist (x *replacements* digit)
        (destructuring-bind (a b d) x
          (f a d)
          (f b d))))))

(defun calibration-value2 (line)
  (parse-digits
   (find-digit line)
   (find-digit line :from-end t)))

(defun part2 (input)
  (reduce #'+ (mapcar #'calibration-value2 (aoc:lines input))))
