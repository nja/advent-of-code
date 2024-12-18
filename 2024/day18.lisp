;;;; day18.lisp

(in-package :aoc2024.day18)

(defun parse (input)
  (mapcar #'aoc:read-integers (aoc:lines input)))

(defun make-map (coordinates)
  (loop with dim = (1+ (reduce #'max (a:flatten coordinates)))
        with map = (make-array (list dim dim) :initial-element nil)
        for i from 0
        for (x y) in coordinates
        do (setf (aref map y x) i)
           (list x y '= i)
        finally (return map)))

(defun neighbours (map s pos)
  (destructuring-bind (x y) pos
    (loop for (dx dy) in '((1 0) (-1 0) (0 1) (0 -1))
          when (safe? map (+ x dx) (+ y dy) s)
            collect (list (+ x dx) (+ y dy)))))

(defun safe? (map x y s)
  (when (array-in-bounds-p map y x)
    (or (null (aref map y x)) (< s (aref map y x)))))

(defun donep (map)
  (let ((goal (mapcar #'1- (reverse (array-dimensions map)))))
    (a:curry #'equal goal)))

(defun part1 (input)
  (let ((map (make-map (parse input))))
    (dijkstra:distance (dijkstra:search* (list 0 0) (a:curry #'neighbours map 1024) :donep (donep map)))))

(defun last-passable-second (map)
  (fork (a:curry #'passable? map) 0 (reduce #'* (array-dimensions map))))

(defun passable? (map s)
  (not (null (dijkstra:search* (list 0 0) (a:curry #'neighbours map s) :donep (donep map)))))

(defun fork (predicate lo hi)
  (if (eql lo hi)
      lo
      (let ((mid (ceiling (+ lo hi) 2)))
        (if (funcall predicate mid)
            (fork predicate mid hi)
            (fork predicate lo (1- mid))))))

(defun part2 (input)
  (let* ((coordinates (parse input)))
    (format nil "~{~a~^,~}" (elt coordinates (1+ (last-passable-second (make-map coordinates)))))))
