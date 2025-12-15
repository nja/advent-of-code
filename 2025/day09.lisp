;;;; day09.lisp

(in-package :aoc2025.day09)

(defun parse (input)
  (mapcar #'aoc:read-as-list (aoc:lines (aoc:tr "," " " input))))

(defun largest-rectangle-area (pairs)
  (let ((max 0))
    (a:map-combinations (lambda (x) (a:maxf max (apply #'rectangle-area x)))
                        pairs :length 2)
    max))

(defun rectangle-area (p q)
  (destructuring-bind ((px py) (qx qy)) (list p q)
    (* (1+ (abs (- px qx))) (1+ (abs (- py qy))))))

(defun part1 (input)
  (largest-rectangle-area (parse input)))

(defun intersections (pairs)
  (flet ((offset (n) (a:rotate (copy-seq pairs) n)))
    (let ((row-intersections (make-hash-table))
          (col-intersections (make-hash-table)))
      (flet ((add (segment intersections)
               (destructuring-bind (d a b) segment
                 (loop for x from (min a b) to (max a b)
                       do (setf (gethash x intersections)
                                (sort (cons d (gethash x intersections)) #'<))))))
        (mapc (lambda (p q r s)
                (multiple-value-bind (segment horizontal?) (shorten p q r s)
                  (add segment (if horizontal? col-intersections row-intersections))))
              pairs (offset -1) (offset -2) (offset -3))
        (values row-intersections col-intersections)))))

(defun left? (&rest pqr)
  (destructuring-bind ((px py) (qx qy) (rx ry)) pqr
    (flet ((north? (y1 y2) (< y2 y1)) (west? (x1 x2) (< x2 x1))
           (south? (y1 y2) (< y1 y2)) (east? (x1 x2) (< x1 x2)))
      (or (and (north? py qy) (west? qx rx))
          (and (east? px qx) (north? qy ry))
          (and (south? py qy) (east? qx rx))
          (and (west? px qx) (south? qy ry))))))

(defun shorten (p q r s)
  (flet ((short (a b) (+ a (signum (- b a)))))
    (multiple-value-bind (d a b horizontal?) (horizontal? q r)
      (values (list d
                    (if (left? p q r) (short a b) a)
                    (if (left? q r s) (short b a) b))
              horizontal?))))

(defun horizontal? (&rest pq)
  (destructuring-bind ((px py) (qx qy)) pq
    (if (= py qy)
        (values py px qx t)
        (values px py qy nil))))

(defun largest-redgreen-area (pairs)
  (multiple-value-bind (row-intersections col-intersections) (intersections pairs)
    (let ((max 0))
      (a:map-combinations (lambda (pq)
                            (when (sides-inside? row-intersections col-intersections pq)
                              (a:maxf max (apply #'rectangle-area pq))))
                          pairs :length 2)
      max)))

(defun sides-inside? (row-intersections col-intersections pq)
  (destructuring-bind ((px py) (qx qy)) pq
    (and (segment-is-inside? col-intersections px (min py qy) (max py qy))
         (segment-is-inside? col-intersections qx (min py qy) (max py qy))
         (segment-is-inside? row-intersections py (min px qx) (max px qx))
         (segment-is-inside? row-intersections qy (min px qx) (max px qx)))))

(defun segment-is-inside? (intersections d a b)
  (and (oddp (count-intersections intersections d 0 (min a b)))
       (zerop (count-intersections intersections d (1+ (min a b)) (1- (max a b))))))

(defun count-intersections (intersections d a b)
  (loop for x in (gethash d intersections)
        count (<= a x b)))

(defun part2 (input)
  (largest-redgreen-area (parse input)))
