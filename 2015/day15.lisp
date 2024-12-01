;;;; day15.lisp

(in-package :aoc2015.day15)

(defun parse (input)
  (mapcar (lambda (line)
            (mapcar #'parse-integer (ppcre:all-matches-as-strings "-?\\d+" line)))
          (aoc:lines input)))

(defun score (cookie)
  (reduce #'* (butlast (mapcar (lambda (n) (if (plusp n) n 0)) cookie))))

(defun teaspoons (n ingredient)
  (mapcar (a:curry #'* n) ingredient))

(defun add (a b)
  (mapcar #'+ a b))

(defun map-proportions (f sum lots)
  (labels ((rec (sum lots acc)
             (if (zerop lots)
                 (funcall f (cons sum acc))
                 (dotimes (i sum)
                   (rec (- sum i) (1- lots) (cons i acc))))))
    (rec sum (1- lots) nil)))

(defun bake (ingredients proportions)
  (reduce #'add (mapcar #'teaspoons proportions ingredients)))

(defun best (ingredients sum test)
  (let ((best 0))
    (map-proportions
     (lambda (proportions)
       (let ((cake (bake ingredients proportions)))
         (when (funcall test cake)
           (setf best (max best (score cake))))))
     sum (length ingredients))
    best))

(defun part1 (input)
  (best (parse input) 100 #'identity))

(defun calories (cake)
  (car (last cake)))

(defun part2 (input)
  (best (parse input) 100 (lambda (cake) (eql 500 (calories cake)))))
