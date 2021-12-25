;;;; day19.lisp

(in-package #:aoc2021.day19)

(defun parse-scanners (input)
  (mapcar #'parse-scanner (aoc:sections input)))

(defun parse-scanner (section)
  (mapcar #'parse-beacon (rest (aoc:lines section))))

(defun parse-beacon (line)
  (apply #'vector (mapcar #'parse-integer (str:split #\, line))))

(defun mul-a (rows cols)
  (loop with result = (make-array (list (array-dimension rows 0)
                                        (array-dimension cols 1)))
        for (row col) in (intersections rows cols)
        for i from 0
        do (setf (row-major-aref result i)
                 (scalar-product-a rows cols row col))
        finally (return result)))

(defun intersections (rows cols)
  (loop for row below (array-dimension rows 0)
        nconc (loop for col below (array-dimension cols 1)
                    collect (list row col))))

(defun scalar-product-a (rows cols row col)
  (loop for i below (array-dimension rows 0)
        sum (* (aref rows row i)
               (aref cols i col))))

(defun mul-v (rows colvec)
  (loop with result = (make-array (array-dimension rows 0))
        for row below (array-dimension rows 0)
        do (setf (aref result row)
                 (scalar-product-v rows colvec row))
        finally (return result)))

(defun scalar-product-v (rows colv row)
  (loop for i below (array-dimension rows 0)
        sum (* (aref rows row i)
               (aref colv i))))

(defun radians (degrees)
  (* degrees (/ pi 180)))

(defmacro def-rotation-matrix (name (cosθ sinθ -sinθ)
                               a b c d e f g h i)
  `(defun ,name (θ)
     (let ((,cosθ (truncate (cos (radians θ))))
           (,sinθ (truncate (sin (radians θ))))
           (,-sinθ (- (truncate (sin (radians θ))))))
       (make-array '(3 3) :initial-contents
                   `((,,a ,,b ,,c)
                     (,,d ,,e ,,f)
                     (,,g ,,h ,,i))))))

(def-rotation-matrix rotation-matrix-x (cosθ sinθ -sinθ)
                     1    0     0
                     0 cosθ -sinθ
                     0 sinθ  cosθ)

(def-rotation-matrix rotation-matrix-y (cosθ sinθ -sinθ)
                      cosθ 0 sinθ
                         0 1    0
                     -sinθ 0 cosθ)

(def-rotation-matrix rotation-matrix-z (cosθ sinθ -sinθ)
                     cosθ -sinθ 0
                     sinθ  cosθ 0
                        0     0 1)

(defun orientation-matrices ()
  (remove-duplicates
   (mapcar (lambda (x) (reduce #'mul-a x))
           (a:map-product
            #'list
            (mapcar #'rotation-matrix-x '(0 90 180 270))
            (mapcar #'rotation-matrix-y '(0 90 180 270))
            (mapcar #'rotation-matrix-z '(0 90 180 270))))
   :test #'equalp))

(defparameter *orientations* (orientation-matrices))

(defun region-rotator (region)
  (let ((orientations *orientations*))
    (lambda ()
      (when orientations
        (mapcar (a:curry #'mul-v (pop orientations)) region)))))

(defun distance (vec-a vec-b)
  (map 'vector #'- vec-a vec-b))

(defun distances (region-a region-b)
  (a:map-product #'distance region-a region-b))

(defun vector-count (vectors)
  (let ((counts (make-hash-table :test 'equalp)))
    (dolist (vector vectors counts)
      (incf (gethash vector counts 0)))))

(defun distance-match (distance-counts)
  (loop with best-distance
        for distance being the hash-keys in distance-counts
          using (hash-value count)
        maximize count into max-count
        when (eql count max-count)
          do (setf best-distance distance)
        finally (when  (>= max-count 12) (return best-distance))))

(defun find-distance (from-region to-region)
  (loop with rotator = (region-rotator to-region)
        for region = (funcall rotator)
        while region
        for distance = (distance-match (vector-count (distances from-region region)))
        when distance return (values region distance)))

(defun transpose-region (region distance)
  (mapcar (a:curry #'add-v distance) region))

(defun add-v (a b)
  (map 'vector #'+ a b))

(defun merge-regions (region-a region-b)
  (multiple-value-bind (oriented-region distance) (find-distance region-a region-b)
    (when oriented-region
      (values (union region-a (transpose-region oriented-region distance) :test #'equalp)
              distance))))

(defun merge-all-regions (regions)
  (loop with merged = (first regions)
        with unmerged = (rest regions)
        with distances
        for next = (pop unmerged)
        while next
        for (match distance) = (multiple-value-list (merge-regions merged next))
        if match
          do (setf merged match)
             (push distance distances)
        else
          do (setf unmerged (append unmerged (list next)))
        finally (return (values merged distances))))

(defun part1 (input)
  (length (merge-all-regions (parse-scanners input))))

(defun manhattan-distance (vec-a vec-b)
  (reduce #'+ (map 'vector (lambda (x y) (abs (- x y))) vec-a vec-b)))

(defun max-manhattan-distance (region)
  (reduce #'max (a:map-product #'manhattan-distance region region)))

(defun part2 (input)
  (max-manhattan-distance (nth-value 1 (merge-all-regions (parse-scanners input)))))
