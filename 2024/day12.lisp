;;;; day12.lisp

(in-package :aoc2024.day12)

(defun set-regions (plots)
  (loop with regions = (make-array (array-dimensions plots))
        for row below (array-dimension plots 0)
        do (loop for col below (array-dimension plots 1)
                 for plant = (aref array row col)
                 for region = (or (aref regions row col) (list (list row col)))
                 
                 )))

(defun range (n)
  (loop for i below n collect i))

(defun indices (array)
  (mapcan (lambda (row) (mapcar (a:curry #'list row) (range (array-dimension array 1))))
          (range (array-dimension array 0))))

(defun regions (map)
  (let ((visited (make-hash-table :test 'equal)))
    (remove nil (mapcar (lambda (x)
                          (unless (gethash x visited)
                            (mapc (lambda (x) (setf (gethash x visited) t))
                                  (region map x))))
                        (indices map)))))

(defun neighbours (map crop)
  (lambda (p)
    (remove nil (mapcar (lambda (n)
                          (and (apply #'array-in-bounds-p map n)
                               (equal crop (apply #'aref map n))
                               n))
                        (mapcar (lambda (d) (mapcar #'+ p d)) '((1 0) (-1 0) (0 1) (0 -1)))))))

(defun region (map start)
  (mapcar #'dijkstra:item (dijkstra:search* start (neighbours map (apply #'aref map start)))))

(defun fences (region)
  (if (null (cdr region))
      4
      (- (* (length region) 4)
         (* (shared-sides region) 2))))

(defun shared-sides (region)
  (let ((count 0))
    (a:map-combinations
     (lambda (p)
       (when (= 1 (apply #'distance p))
         (incf count)))
     region :length 2)
    count))

(defun distance (a b)
  (reduce #'+ (mapcar (a:compose #'abs #'-) a b)))

(defun price (region)
  (* (length region) (fences region)))

(defun part1 (input)
  (reduce #'+ (mapcar #'price (regions (aoc:to-array input)))))
;;; 1431316
(defparameter *test*
"AAAA
BBCD
BBCC
EEEC")