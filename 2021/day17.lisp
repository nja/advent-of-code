;;;; day17.lisp

(in-package #:aoc2021.day17)

(defun parse-target-area (input)
  (ppcre:register-groups-bind ((#'parse-integer x1 x2 y1 y2))
      ("target area: x=(-?\\d+)\\.\\.(-?\\d+), y=(-?\\d+)\\.\\.(-?\\d+)" input)
    (list x1 x2 y1 y2)))

(defun hit (x1 x2 y1 y2)
  (let ((min-x (min x1 x2)) (max-x (max x1 x2))
        (min-y (min y1 y2)) (max-y (max y1 y2)))
    (lambda (x y)
     (and (<= min-x x max-x) (<= min-y y max-y)))))

(defun gone (x1 x2 y1 y2)
  (let ((max-x (max x1 x2))
        (min-y (min y1 y2)))
    (lambda (x y)
      (or (< max-x x) (< y min-y)))))

(defun step* (vx vy)
  (let ((x 0) (y 0))
    (lambda ()
      (incf x vx)
      (incf y vy)
      (incf vx (cond ((> vx 0) -1)
                     ((< vx 0) 1)
                     (t 0)))
      (decf vy)
      (list x y))))

(defun result (x1 x2 y1 y2)
  (let ((hit? (hit x1 x2 y1 y2))
        (gone? (gone x1 x2 y1 y2)))
    (lambda (x y)
      (loop with step = (step* x y)
            for (x y) = (funcall step)
            for hit = (funcall hit? x y)
            for gone = (funcall gone? x y)
            maximize y into max-y
            until (or hit gone)
            finally (return (list hit max-y))))))

(defun hit? (x)
  (first x))

(defun compare (a b)
  (or (compare-hit a b) (and (hit? a) (hit? b) (compare-height a b))))

(defun compare-hit (a b)
  (and (hit? a) (not (hit? b))))

(defun compare-height (a b)
  (> (second a) (second b)))

(defun scan-from (x y result n)
  (loop with hits
        for x from x upto n do
        (loop for y from y upto n
              for attempt = (funcall result x y)
              when (hit? attempt)
                do (push (list attempt (list x y)) hits))
        finally (return (sort hits #'compare :key #'first))))

(defun part1 (input)
  (cadaar (scan-from 0 0 (apply #'result (parse-target-area input)) 200)))

(defun part2 (input)
  (length (scan-from 0 -199 (apply #'result (parse-target-area input)) 200)))
