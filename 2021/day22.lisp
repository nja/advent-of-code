;;;; day22.lisp

(in-package #:aoc2021.day22)

(defstruct (cuboid (:conc-name) (:constructor cuboid (x1 x2 y1 y2 z1 z2)))
  (x1 0 :type fixnum) (x2 0 :type fixnum)
  (y1 0 :type fixnum) (y2 0 :type fixnum)
  (z1 0 :type fixnum) (z2 0 :type fixnum))

(defun parse-step (line)
  (ppcre:register-groups-bind (((aoc:symbols '(on off)) action)
                               (#'parse-integer x1 x2 y1 y2 z1 z2))
      ("(on|off) x=(-?\\d+)\\.\\.(-?\\d+),y=(-?\\d+)\\.\\.(-?\\d+),z=(-?\\d+)\\.\\.(-?\\d+)" line)
    (list action (cuboid x1 x2 y1 y2 z1 z2))))

(defun parse-steps (input)
  (mapcar #'parse-step (aoc:lines input)))

(defun void? (c)
  (or (< (x2 c) (x1 c))
      (< (y2 c) (y1 c))
      (< (z2 c) (z1 c))))

(defun intersect (a b)
  (let ((i (cuboid (max (x1 a) (x1 b)) (min (x2 a) (x2 b))
                   (max (y1 a) (y1 b)) (min (y2 a) (y2 b))
                   (max (z1 a) (z1 b)) (min (z2 a) (z2 b)))))
    (unless (void? i) i)))

(defun under (c i) (cuboid     (x1 c)     (x2 c)     (y1 c)     (y2 c)     (z1 c) (1- (z1 i))))
(defun over  (c i) (cuboid     (x1 c)     (x2 c)     (y1 c)     (y2 c) (1+ (z2 i))    (z2 c)))
(defun left  (c i) (cuboid     (x1 c)     (x2 c)     (y1 c) (1- (y1 i))    (z1 i)     (z2 i)))
(defun right (c i) (cuboid     (x1 c)     (x2 c) (1+ (y2 i))    (y2 c)     (z1 i)     (z2 i)))
(defun back  (c i) (cuboid     (x1 c) (1- (x1 i))    (y1 i)     (y2 i)     (z1 i)     (z2 i)))
(defun front (c i) (cuboid (1+ (x2 i))    (x2 c)     (y1 i)     (y2 i)     (z1 i)     (z2 i)))

(defun remove-intersection (a b)
  (let ((i (intersect a b)))
    (if i
        (delete-if #'void? (list (under a i) (over a i)
                                 (left a i) (right a i)
                                 (back a i) (front a i)))
        (list a))))

(defun volume (c)
  (if (void? c)
      0
      (* (1+ (abs (- (x2 c) (x1 c))))
         (1+ (abs (- (y2 c) (y1 c))))
         (1+ (abs (- (z1 c) (z2 c)))))))

(defun sum-volumes (set)
  (reduce #'+ (mapcar #'volume set)))

(defun on (set on)
  (cons on (mapcan (a:rcurry #'remove-intersection on) set)))

(defun off (set off)
  (mapcan (a:rcurry #'remove-intersection off) set))

(defun process-step (state step)
  (funcall (first step) state (second step)))

(defun process-steps (steps)
  (reduce #'process-step steps :initial-value nil))

(defun in-region? (c)
  (every (a:compose (a:rcurry #'<= 50) #'abs)
         (list (x1 c) (x2 c) (y1 c) (y2 c) (z1 c) (z2 c))))

(defun part1 (input)
  (sum-volumes (process-steps (remove-if-not #'in-region? (parse-steps input)
                                             :key #'cadr))))

(defun part2 (input)
  (sum-volumes (process-steps (parse-steps input))))
