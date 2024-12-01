;;;; day10.lisp

(in-package :aoc2019.day10)

(defun asteroids (input)
  (loop with cols = (length (first (aoc:lines input)))
        for c across (remove #\Newline input)
        for i from 0
        unless (eql #\. c)
          collect (multiple-value-list (truncate i cols))))

(defun angles (from asteroids)
  (mapcar (a:curry #'angle from) asteroids))

(defun angle (a b)
  (let* ((d (sub a b))
         (gcd (apply #'gcd d)))
    (if (zerop gcd)
        d
        (mapcar (a:rcurry #'/ gcd) d))))

(defun sub (a b)
  (mapcar #'- a b))

(defun asteroid-count (from asteroids)
  (1- (length (remove-duplicates (angles from asteroids) :test #'equal))))

(defun part1 (input)
  (let ((asteroids (asteroids input)))
    (reduce #'max (mapcar (a:rcurry #'asteroid-count asteroids) asteroids))))

(defun distance (a b)
  (reduce #'+ (mapcar #'abs (sub a b))))

(defun radians (y x)
  (atan (coerce y (type-of pi)) x))

(defun degrees (radians)
  (* radians 180 (/ 1 pi)))

(defun laser (angle)
  (mod (- (degrees (apply #'radians angle)) 90) 360))

(defun vaporize (from asteroids)
  (let ((asteroids (remove from asteroids :test #'equal))
        (hash (make-hash-table)))
    (dolist (asteroid (sort asteroids #'> :key (a:curry #'distance from)))
      (push asteroid (gethash (laser (angle from asteroid)) hash)))
    (loop with vaporized
          until (zerop (hash-table-count hash))
          do (dolist (key (sort (a:hash-table-keys hash) #'<))
               (let ((asteroid (pop (gethash key hash))))
                 (if asteroid
                     (push asteroid vaporized)
                     (remhash key hash))))
          finally (return (nreverse vaporized)))))

(defun monitoring-station (asteroids)
  (first (sort (copy-list asteroids) #'>
               :key (a:rcurry #'asteroid-count asteroids))))

(defun part2 (input)
  (let* ((asteroids (asteroids input))
         (from (monitoring-station asteroids))
         (vaporized (vaporize from asteroids)))
    (destructuring-bind (y x) (nth 199 vaporized)
      (+ (* 100 x) y))))
