;;;; day04.lisp

(in-package :aoc2024.day04)

(defun to-array (input)
  (let* ((lines (aoc:lines input))
         (dim (max (reduce #'max (mapcar #'length lines))
                   (length lines)))
         (array (make-array (list dim dim) :initial-element #\Space)))
    (mapc (lambda (row line)
            (map nil (lambda (col x)
                       (setf (aref array row col) x))
                 (indices line)
                 line))
          (indices lines)
          lines)
    array))

(defun row-indices (array)
  (loop for i below (array-dimension array 0)
        collect i))

(defun col-indices (array)
  (loop for i below (array-dimension array 1)
        collect i))

(defun all-indices (array)
  (mapcan (lambda (row)
            (mapcar (a:curry #'list row) (col-indices array)))
          (row-indices array)))

(defun walk-from (start d in-bounds?)
  (loop for x = start then (add x d)
        while (funcall in-bounds? x)
        collect x))

(defun add (a b)
  (mapcar #'+ a b))

(defun edge (array)
  (remove-if-not (lambda (x)
                   (destructuring-bind (row col) x
                     (or (zerop row)
                         (zerop col)
                         (eql row (1- (array-dimension array 0)))
                         (eql col (1- (array-dimension array 1))))))
                 (all-indices array)))

(defun paths (array)
  (flet ((in-bounds? (x) (apply #'array-in-bounds-p array x)))
    (mapcan (lambda (start)
              (remove nil
                      (mapcar (lambda (d)
                                (unless (in-bounds? (add start (mapcar #'- d)))
                                  (walk-from start d #'in-bounds?)))
                              '((-1 -1) (-1 0) (-1 1) (0 -1) (0 1) (1 -1) (1 0) (1 1)))))
            (edge array))))

(defun indices (seq)
  (loop for i below (length seq)
        collect i))

(defun collector (length)
  (let (things)
    (lambda (x)
      (if (characterp x)
          (push x things)
          (setf things nil))
      (loop for i from 1
            for c in things
            collect c into chars
            when (equal i length)
              return (reverse (map 'string #'identity chars))))))

(defun words (array length)
  (mapcan (lambda (path)
            (let ((collector (collector length)))
              (remove nil (mapcar (lambda (subscripts)
                                    (funcall collector (apply #'aref array subscripts)))
                                  path))))
          (paths array)))

(defun part1 (input)
  (count-if (a:curry #'string= "XMAS") (words (to-array input) 4)))

(defun map-cross (f array)
  (mapcar (lambda (x)
            (destructuring-bind (row col) x
                (flet ((a (ro co) (sref array (+ row ro) (+ col co))))
                  (funcall f
                           (a -1 -1)
                           (a  1 -1)
                           (a  0  0)
                           (a -1  1)
                           (a  1  1)))))
          (all-indices array)))

(defun sref (array &rest subscripts)
  (when (apply #'array-in-bounds-p array subscripts)
    (apply #'aref array subscripts)))

(defun x-mas? (tl bl c tr br)
  (labels ((str (&rest args) (format nil "~{~a~}" args))
           (mas? (s) (find s '("MAS") :test #'string=))
           (sam? (s) (find s '("SAM") :test #'string=))
           (same? (&rest things) (or (every #'mas? things)
                                     (every #'sam? things))))
    (let ((a (str tl c br))
          (b (str bl c tr))
          (c (str tr c bl)))
      (or (same? a b)
          (same? a c)))))

(defun part2 (input)
  (count-if-not #'null (map-cross #'x-mas? (to-array input))))
