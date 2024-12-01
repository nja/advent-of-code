;;;; day18.lisp

(in-package :aoc2022.day18)

(defun parse (input)
  (mapcar (lambda (line)
            (read-from-string (format nil "(~a)" (aoc:tr "," " " line))))
          (aoc:lines input)))

(defun add (a b)
  (mapcar #'+ a b))

(defun to-hash (cubes)
  (let ((hash (make-hash-table :test 'equal)))
    (dolist (c cubes hash)
      (setf (gethash c hash) t))))

(defun counter (hash sides)
  (lambda (d) (count-if (lambda (c) (gethash (add c d) hash)) sides)))

(defun part1 (input)
  (let ((cubes (parse input)))
    (- (* 6 (length cubes))
       (* 2 (reduce #'+ (mapcar (counter (to-hash cubes) '((1 0 0) (0 1 0) (0 0 1)))
                                cubes))))))

(defun bounds (cubes)
  (list (1- (reduce #'min (a:flatten cubes)))
        (1+ (reduce #'max (a:flatten cubes)))))

(defun steam (hash)
  (destructuring-bind (min max) (bounds (a:hash-table-keys hash))
    (mapcar #'d:item (d:search* (list min min min) (neighbours hash min max)))))

(defun neighbours (hash min max)
  (lambda (c)
    (let ((surrounding (mapcar (a:curry #'add c)
                               '((1 0 0) (0 1 0) (0 0 1)
                                 (-1 0 0) (0 -1 0) (0 0 -1)))))
      (remove-if (lambda (c)
                   (or (gethash c hash)
                       (some (lambda (x) (not (<= min x max))) c)))
                 surrounding))))

(defun part2 (input)
  (let ((hash (to-hash (parse input))))
    (reduce #'+ (mapcar (counter hash
                                 '((1 0 0) (0 1 0) (0 0 1)
                                   (-1 0 0) (0 -1 0) (0 0 -1)))
                        (steam hash)))))
