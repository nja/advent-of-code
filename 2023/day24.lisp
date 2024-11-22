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
                (+ (y a) (* (dy a) u))))))))

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

(defun tick (rays)
  (map 'vector
       (lambda (ray)
         (ray (+ (x ray) (dx ray))
              (+ (y ray) (dy ray))
              (+ (z ray) (dz ray))
              (dx ray)
              (dy ray)
              (dz ray)))
       rays))

(defun distances (a b hash)
  (loop for i below (length a)
        for ray = (aref a i) do
          (loop for j below (length b)
                unless (eql i j)
                  do (incf (gethash (simplify (distance ray (aref b j))) hash 0)))))

(defun steps (rays n)
  (dotimes (i n rays)
    (setf rays (tick rays))))

(defun distance (a b)
  (list (- (x b) (x a))
        (- (y b) (y a))
        (- (z b) (z a))))

(defun simplify (x)
  (mapcar (a:rcurry #'/ (apply #'gcd x)) x))

(defun survey-distances (rays n step)
  (let ((hash (make-hash-table :test 'equal))
        (rays (coerce rays 'vector)))
    (dotimes (i n)
      (distances rays (setf rays (steps rays step)) hash))
    (remove 1 (sort (a:hash-table-alist hash) #'> :key #'cdr)
                                  :key #'cdr)))

(defun meta-survey (rays n)
  (loop for step from 1 to 10
        for result = (survey-distances rays n step)
        when result return (values result step)))