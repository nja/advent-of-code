;;;; day24.lisp

(in-package :aoc2023.day24)

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

;;; x + t*dx = x0 + t*dx0
;;; t = (x0 - x) / (dx - dx0)
;;;
;;; (x0 - x) / (dx - dx0) = (y0 - y) / (dy - dy0)
;;; (x0 - x) / (dx - dx0) = (z0 - z) / (dz - dz0)
;;;
;;; dy(x0 - x) - dy0(x0 - x) = dx(y0 - y) - dx0(y0 - y)
;;; x0dy - dyx - x0dy0 + dy0x = y0dx - dxy - dx0y0 + dx0y
;;; x0dz - dzx - x0dz0 + dz0x = z0dx - dxz - dx0z0 + dx0z
;;;
;;; dy0x - dx0y - y0dx + x0dy = x0dy0 - dx0y0 + dyx - dxy
;;; dz0x - dx0z - z0dx + x0dz = x0dz0 - dx0z0 + dzx - dxz
;;; dynx - dxny - yndx + xndy = xndyn - dxnyn + dyx - dxy
;;; dznx - dxnz - zndx + xndz = xndzn - dxnzn + dzx - dxz
;;;
;;; x(dy0 - dyn) + y(dxn - dx0) + dx(yn - y0) + dy(x0 - xn) = x0dy0 - dx0y0 + xndyn - dxnyn
;;; x(dz0 - dzn) + z(dxn - dx0) + dx(zn - z0) + dz(x0 - xn) = x0dz0 - dx0z0 + xndzn - dxnzn

(defun equation-pair (h0 hn)
  (let ((x0 (x h0)) (xn (x hn))
        (y0 (y h0)) (yn (y hn))
        (dx0 (dx h0)) (dxn (dx hn))
        (dy0 (dy h0)) (dyn (dy hn))
        (z0 (z h0)) (zn (z hn))
        (dz0 (dz h0)) (dzn (dz hn)))
    (values (list (list (- dy0 dyn) (- dxn dx0) 0 (- yn y0) (- x0 xn) 0)
                  (list (- dz0 dzn) 0 (- dxn dx0) (- zn z0) 0 (- x0 xn)))
            (list (+ (* dxn yn) (* x0 dy0) (- (* dx0 y0)) (- (* xn dyn)))
                  (+ (* dxn zn) (* x0 dz0) (- (* dx0 z0)) (- (* xn dzn)))))))

(defun equation-system (h0 h1 h2 h3 &rest rest)
  (declare (ignore rest))
  (let (coefficients constants)
    (mapc (lambda (x)
            (multiple-value-bind (cs ts) (equation-pair h0 x)
              (setf coefficients (append coefficients cs)
                    constants (append constants ts))))
          (list h1 h2 h3))
    (values coefficients constants)))

(defun part2 (input)
  (destructuring-bind (x y z dx dy dz)
      (multiple-value-call #'lalg:cramers (apply #'equation-system (parse input)))
    (declare (ignore dx dy dz))
    (reduce #'+ (list x y z))))
