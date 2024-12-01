;;;; day14.lisp

(in-package :aoc2015.day14)

(defstruct (reindeer :conc-name) name velocity duration resting)

(defun parse (input)
  (mapcar
   (lambda (line)
     (ppcre:register-groups-bind (name (#'parse-integer velocity duration resting))
         ("(\\w+) can fly (\\d+) km/s for (\\d+) seconds, but then must rest for (\\d+) seconds." line)
       (make-reindeer :name name :velocity velocity :duration duration :resting resting)))
   (aoc:lines input)))

(defun traveled (reindeer seconds)
  (let ((period (+ (duration reindeer) (resting reindeer))))
    (multiple-value-bind (periods remainder) (truncate seconds period)
      (+ (* periods (duration reindeer) (velocity reindeer))
         (* (velocity reindeer) (min (duration reindeer) remainder))))))

(defun part1 (input)
  (reduce #'max (mapcar (a:rcurry #'traveled 2503) (parse input))))

(defun race (reindeer seconds)
  (loop with scores = (make-hash-table)
        for second from 1 to seconds
        for leaders = (leaders reindeer second)
        do (dolist (leader leaders)
             (incf (gethash leader scores 0)))
        finally (return scores)))

(defun leaders (reindeer second)
  (let* ((scores (mapcar (lambda (r) (list (traveled r second) r)) reindeer))
         (best (reduce #'max scores :key #'car))
         (leaders (remove best scores :key #'car :test-not #'eql)))
    (mapcar #'cadr leaders)))

(defun part2 (input)
  (reduce #'max (a:hash-table-values (race (parse input) 2503))))
