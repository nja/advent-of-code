;;;; day12.lisp

(in-package :aoc2024.day12)

(defun set-regions (plots)
  (loop with regions = (make-array (array-dimensions plots))
        for row below (array-dimension plots 0)
        do (loop for col below (array-dimension plots 1)
                 for plant = (aref regions row col)
                 for region = (or (aref regions row col) (list (list row col))))))

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

(defun fence-top (map region row)
  (let ((ins (ins region)))
    (flet ((in? (row col) (gethash (list row col) ins)))
      (loop for col from 0 below (array-dimension map 1)
            for pf = f
            for f = (and (in? row col) (not (in? (1- row) col)))
            count (and f (not pf))))))

(defun fence-bottom (map region row)
  (let ((ins (ins region)))
    (flet ((in? (row col) (gethash (list row col) ins)))
      (loop for col from 0 below (array-dimension map 1)
            for pf = f
            for f = (and (in? row col) (not (in? (1+ row) col)))
            count (and f (not pf))))))

(defun fence-left (map region col)
  (let ((ins (ins region)))
    (flet ((in? (row col) (gethash (list row col) ins)))
      (loop for row from 0 below (array-dimension map 0)
            for pf = f
            for f = (and (in? row col) (not (in? row (1- col))))
            count (and f (not pf))))))

(defun fence-right (map region col)
  (let ((ins (ins region)))
    (flet ((in? (row col) (gethash (list row col) ins)))
      (loop for row from 0 below (array-dimension map 0)
            for pf = f
            for f = (and (in? row col) (not (in? row (1+ col))))
            count (and f (not pf))))))

(defun ins (region)
  (let ((ins (make-hash-table :test 'equal)))
    (dolist (x region ins)
      (setf (gethash x ins) t))))

(defun price2 (map region)
  (* (length region)
     (fences-2 map region)))

(defun fences-2 (map region)
  (+ (loop for row below (array-dimension map 0)
           sum (fence-top map region row)
           sum (fence-bottom map region row))
     (loop for col below (array-dimension map 1)
           sum (fence-left map region col)
           sum (fence-right map region col))))

(defun part2 (input)
  (let ((map (aoc:to-array input)))
   (reduce #'+ (mapcar (a:curry #'price2 map) (regions map)))))
