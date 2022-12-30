;;;; day04.lisp

(in-package :aoc2019.day04)

(defun parse (input)
  (mapcar #'parse-integer (str:split "-" input)))

(defun map-increasing-digits (f min max)
  (loop for x from min to max
        when (never-decrease? x)
          do (funcall f x)))

(defun never-decrease? (x)
  (if (zerop x)
      t
      (multiple-value-bind (tens ones) (truncate x 10)
        (when (<= (mod tens 10) ones)
          (never-decrease? tens)))))

(defun pair? (x)
  (when (< 9 x)
    (multiple-value-bind (tens ones) (truncate x 10)
      (if (eql (mod tens 10) ones)
          t
          (pair? tens)))))

(defun count-passwords (test min max)
  (let ((count 0))
    (map-increasing-digits (lambda (x)
                             (when (funcall test x)
                               (incf count)))
                           min max)
    count))

(defun part1 (input)
  (apply #'count-passwords #'pair? (parse input)))

(defun strict-pair? (x)
  (let ((digits (format nil "~d" x)))
    (flet ((at (i)
             (when (array-in-bounds-p digits i)
               (aref digits i))))
      (loop for i below (length digits)
            for d = (at i)
            when (and (eql d (at (1+ i)))
                      (not (eql d (at (+ i 2))))
                      (not (eql d (at (1- i)))))
              return t))))

(defun part2 (input)
  (apply #'count-passwords #'strict-pair? (parse input)))
