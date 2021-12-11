;;;; day15.lisp

(in-package #:aoc2018.day15)

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

(defparameter *input* (to-array (aoc:lines (aoc:input-for 2018 15))))
(defparameter *test* (to-array (aoc:lines

"#########
#G..G..G#
#.......#
#.......#
#G..E..G#
#.......#
#.......#
#G..G..G#
#########")))

(defun amap (f array)
  (let ((copy (make-array (array-dimensions array))))
    (loop for row below (array-dimension array 0) do
      (loop for col below (array-dimension array 1)
            do (setf (aref copy row col)
                     (funcall f (aref array row col)))))
    copy))

(defun asubstitute (new old array)
  (amap (lambda (x)
          (if (equal old x)
              new
              x))
        array))

(defun init-fill (array ch)
  (asubstitute 0 ch array))

(defun aproc (f array)
  (let ((rows (array-dimension array 0))
        (cols (array-dimension array 1))
        (updates 0))
    (labels ((sref (r c)
               (when (and (< -1 r rows)
                          (< -1 c cols))
                 (aref array r c)))
             (near (r c)
               (delete nil (list (sref (1+ r) c)
                                 (sref (1- r) c)
                                 (sref r (1+ c))
                                 (sref r (1- c))))))
      (loop for row below rows do
        (loop for col below cols
              for old = (aref array row col)
              for new = (apply f old (near row col))
              when new do (setf (aref array row col) new)
                (incf updates)))
      (values array updates))))

(defun filler (x &rest near)
  (let ((numbers (remove-if-not #'numberp near)))
    (alexandria:when-let ((min (when numbers
                                 (loop for n in numbers
                                       when (integerp n)
                                         minimize n))))
      (cond ((eql x #\.) (1+ min))
            ((integerp x) (when (< (1+ min) x) (1+ min)))))))

(defun aformat (x)
  (cond ((null x) #\?)
        ((integerp x) (if (< x 36)
                          (char-downcase (digit-char x 36))
                          #\!))
        (t x)))

(defun afill (array)
  (multiple-value-bind (a updates) (aproc #'filler array)
    (values a updates)))

(defun lopety (array)
  (loop for (a u) = (multiple-value-list (afill array))
        do (aoc:print-array a #'aformat)
        while (< 0 u)
        finally (return a)))