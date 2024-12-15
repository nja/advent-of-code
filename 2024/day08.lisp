;;;; day08.lisp

(in-package :aoc2024.day08)

(defun frequency (character)
  (or (digit-char-p character)
      (intern (format nil "~a" character) (symbol-package 'frequency))))

(defun antennas (array)
  (loop with antennas
        for row below (array-dimension array 0)
        do (loop for col below (array-dimension array 1)
                 for f = (aref array row col)
                 unless (find f ".#")
                   do (push (list (frequency f) col row)
                            (getf antennas (frequency f) nil)))
        finally (return (remove-if #'atom antennas))))

(defun anti-nodes (antennas)
  (let (result)
    (a:map-permutations
     (lambda (p)
       (destructuring-bind ((f1 x1 y1) (f2 x2 y2)) p
         (declare (ignore f1 f2))
         (push (anti-node x1 y1 x2 y2) result)))
     antennas
     :length 2)
    result))

(defun anti-node (x1 y1 x2 y2)
  (list (+ x1 (- x1 x2))
        (+ y1 (- y1 y2))))

(defun count-antinodes (array antinodes)
  (length
   (remove-duplicates
    (remove-if-not (lambda (x) (apply #'array-in-bounds-p array x))
                   antinodes)
    :test 'equal)))

(defun part1 (input)
  (let* ((array (aoc:to-array input))
         (antennas (antennas array))
         (anti-nodes (mapcan #'anti-nodes antennas)))
    (count-antinodes array anti-nodes)))

(defun dx (x1 y1 x2 y2)
  (* (signum (- x2 x1))
     (if (= y1 y2)
         1
         (numerator (/ (abs (- x2 x1))
                       (abs (- y2 y1)))))))

(defun map-line (in-bounds-p x1 y1 x2 y2 f)
  (unless (and (= x1 x2) (= y1 y2))
    (let ((dx (dx x1 y1 x2 y2))
          (dy (dx y1 x1 y2 x2)))
      (loop for x = x1 then (+ x dx)
            for y = y1 then (+ y dy)
            while (funcall in-bounds-p x y)
            do (funcall f x y)))))

(defun map-antenna-lines (antennas in-bounds-p f)
  (a:map-permutations
   (lambda (p)
     (destructuring-bind ((f1 x1 y1) (f2 x2 y2)) p
       (declare (ignore f1 f2))
       (map-line in-bounds-p x1 y1 x2 y2 f)))
   antennas
   :length 2))

(defun part2 (input)
  (let* ((array (aoc:to-array input))
         (results (make-array (array-dimensions array) :initial-element nil)))
    (dolist (antennas (antennas array))
      (map-antenna-lines antennas
                         (lambda (x y) (array-in-bounds-p array y x))
                         (lambda (x y) (setf (aref results y x) t))))
    (loop for i below (array-total-size results)
          count (row-major-aref results i))))
