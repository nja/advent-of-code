;;;; day12.lisp

(in-package :aoc2019.day12)

(defun integers (line)
  (mapcar #'parse-integer (ppcre:all-matches-as-strings "-?\\d+" line)))

(defun parse (input)
  (mapcar (a:compose (a:curry #'apply #'moon) #'integers) (aoc:lines input)))

(defstruct (moon :conc-name (:constructor moon (x y z)))
  x y z (vx 0) (vy 0) (vz 0))

(defun gravity (moon body)
  (flet ((force (a b) (signum (- b a))))
    (incf (vx moon) (force (x moon) (x body)))
    (incf (vy moon) (force (y moon) (y body)))
    (incf (vz moon) (force (z moon) (z body)))))

(defun velocity (moon)
  (incf (x moon) (vx moon))
  (incf (y moon) (vy moon))
  (incf (z moon) (vz moon)))

(defun time-step (moons)
  (mapc (lambda (moon)
          (mapc (a:curry #'gravity moon) moons))
        moons)
  (mapc #'velocity moons))

(defun potential-energy (moon)
  (+ (abs (x moon)) (abs (y moon)) (abs (z moon))))

(defun kinetic-energy (moon)
  (+ (abs (vx moon)) (abs (vy moon)) (abs (vz moon))))

(defun total-energy (moon)
  (* (potential-energy moon) (kinetic-energy moon)))

(defun system-energy (moons)
  (reduce #'+ (mapcar #'total-energy moons)))

(defun part1 (input)
  (loop with moons = (parse input)
        repeat 1000
        do (time-step moons)
        finally (return (system-energy moons))))

(defun cycles (moons)
  (loop with start = (mapcar #'copy-moon moons)
        for i from 1
        do (time-step moons)
        until (every #'equalp moons start)
        finally (return i)))

(defun single-axis (moons get set)
  (mapcar (lambda (x)
            (let ((copy (moon 0 0 0)))
              (funcall set (funcall get x) copy)
              copy))
          moons))

(defun axis-cycles (moons)
  (mapcar (lambda (get set) (cycles (single-axis moons get set)))
          (list #'x #'y #'z)
          (list #'(setf x) #'(setf y) #'(setf z))))

(defun part2 (input)
  (apply #'lcm (axis-cycles (parse input))))
