;;;; day24.lisp

(in-package :aoc2023.day24)

(defparameter *test*
"19, 13, 30 @ -2,  1, -2
18, 19, 22 @ -1, -1, -2
20, 25, 34 @ -2, -2, -4
12, 31, 28 @ -1, -2, -1
20, 19, 15 @  1, -5, -3")

(defstruct (ray :conc-name (:constructor ray (x y z dx dy dz)))
  x y z dx dy dz)

(defun parse (input)
  (mapcar (lambda (line)
            (apply #'ray (mapcar #'parse-integer (ppcre:all-matches-as-strings "-?\\d+" line))))
          (aoc:lines input)))

(defun 2donly (rays)
  (dolist (ray rays rays)
    (setf (z ray) 0
          (dz ray) 0)))

(defun 2d-intersect? (a b)
  (let ((dx (- (x b) (x a)))
        (dy (- (y b) (y a)))
        (det (- (* (dx b) (dy a))
                (* (dy b) (dx a)))))
    (unless (zerop det)
      (let ((u (/ (- (* dy (dx b))
                     (* dx (dy b)))
                  det))
            (v (/ (- (* dy (dx a))
                     (* dx (dy a)))
                  det)))
        (when (and (plusp u) (plusp v))
          (list (+ (x a) (* (dx a) u))
                (+ (y a) (* (dy a) u))
                ;; (+ (x b) (* (dx b) v))
                ;; (+ (y b) (* (dy b) v))
                ))))))

(defun 2d-intersections (rays)
  (let (result)
    (a:map-combinations (lambda (x)
                          (a:when-let (p (apply #'2d-intersect? x))
                            (push p result)))
                        rays :length 2)
    result))

(defun bounds-predicate (x y)
  (lambda (point)
    (every (lambda (n) (<= x n y)) point)))

(defun part1 (input)
  (count-if (bounds-predicate 200000000000000 400000000000000)
            (2d-intersections (parse input))))