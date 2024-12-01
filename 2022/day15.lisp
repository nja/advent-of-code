;;;; day15.lisp

(in-package :aoc2022.day15)

(defun parse-line (line)
  (let ((clean (ppcre:regex-replace-all "[^-\\d]" line " ")))
    (with-input-from-string (*standard-input* clean)
      (loop repeat 2 collect (list (read) (read))))))

(defun parse (input)
  (mapcar #'parse-line (aoc:lines input)))

(defun pos (x y) (list x y))
(defun add (a b) (mapcar #'+ a b))
(defun x (pos) (car pos))
(defun y (pos) (cadr pos))

(defun manhattan-distance (a b)
  (reduce #'+ (mapcar (a:compose #'abs #'-) a b)))

(defun beacon (d pos) (cons d pos))
(defun beacon-distance (beacon) (car beacon))
(defun beacon-pos (beacon) (cdr beacon))

(defun undetected? (pos beacon)
  (< (beacon-distance beacon)
     (manhattan-distance pos (beacon-pos beacon))))

(defun detected? (pos beacon)
  (<= (manhattan-distance pos (beacon-pos beacon))
      (beacon-distance beacon)))

(defun make-beacons (pairs)
  (mapcar (lambda (pair)
            (destructuring-bind (s b) pair
              (beacon (manhattan-distance s b) b)))
          pairs))

(defun make-sensors (pairs)
  (mapcar (lambda (pair)
            (destructuring-bind (s b) pair
              (beacon (manhattan-distance s b) s)))
          pairs))

(defun extremes (pairs)
  (reduce (lambda (a b)
            (list (reduce #'min (mapcar #'min a b))
                  (reduce #'max (mapcar #'max a b))))
          (mapcar (lambda (pair)
                    (destructuring-bind (s b) pair
                      (let ((d (manhattan-distance s b)))
                        (list (- (x s) d) (+ d (x s))
                              (- (y s) d) (+ d (y s))))))
                  pairs)))

(defun make-occupied (pairs)
  (let ((occupied (make-hash-table :test 'equal)))
    (dolist (pair pairs)
      (setf (gethash (first pair) occupied) t)
      (setf (gethash (second pair) occupied) t))
    (lambda (x y)
      (gethash (pos x y) occupied))))

(defun sweep (sensors occupied? row from to)
  (loop for col from from to to
        for x =  (and
                  (not (funcall occupied? col row))
                  (some (a:curry #'detected? (pos col row)) sensors))
        count x))

(defun part1 (input)
  (apply #'sweep (make-sensors (parse input)) (make-occupied (parse input))
         2000000 (extremes (parse input))))

(defun tuning-frequency (x y)
  (+ (* x 4000000) y))

(defun row-ranges (row sensors)
  (sort (remove nil (mapcar (lambda (sensor)
                              (destructuring-bind (d x y) sensor
                                (let ((yd (abs (- row y))))
                                  (when (<= yd d)
                                    (list (- x (- d yd)) (+ x (- d yd)))))))
                            sensors))
        #'< :key #'car))

(defun sweep2 (sensors occupied? row from to)
  (declare (ignore occupied?))
  (loop with ranges = (row-ranges row sensors)
        for x = from then next
        for next = (next-x x ranges)
        while (<= x to)
        when (eql x next)
          do (return x)))

(defun next-x (x ranges)
  (reduce #'max (mapcar (lambda (r)
                          (destructuring-bind (min max) r
                            (if (<= min x max)
                                (1+ max)
                                x)))
                        ranges)))

(defun scan (sensors occupied? from to)
  (loop for row from from upto to
        for col = (sweep2 sensors occupied? row from to)
        when col do (return (pos col row))))

(defun part2 (input)
  (apply #'tuning-frequency
         (scan (make-sensors (parse input)) (make-occupied (parse input)) 0 4000000)))
