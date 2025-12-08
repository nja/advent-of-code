;;;; day08.lisp

(in-package :aoc2025.day08)

(defun parse (input)
  (mapcar #'aoc:read-as-list (aoc:lines (aoc:tr "," " " input))))

(defun distance (a b)
  (sqrt (reduce #'+ (mapcar (lambda (p q) (expt (- p q) 2)) a b))))

(defun distance-pairs (list)
  (let (pairs)
    (a:map-combinations (lambda (x) (push (cons (apply #'distance x) x) pairs)) list :length 2)
    (mapcar #'cdr (sort pairs #'< :key #'car))))

(defun connect (a b circuits)
  (merge-connected (cons (list a b) circuits)))

(defun merge-connected (circuits)
  (when circuits
    (loop with connected = (first circuits)
          for x in (merge-connected (rest circuits))
          if (intersection connected x)
            do (setf connected (union connected x))
          else
            collect x into unconnected
          finally (return (cons connected unconnected)))))

(defun make-connections (list n)
  (loop repeat n
        for (a b) in (distance-pairs list)
        for circuits = (connect a b (or circuits (mapcar #'list list)))
        finally (return circuits)))

(defun score (circuits)
  (reduce #'* (subseq (sort (mapcar #'length circuits) #'>) 0 3)))

(defun part1 (input)
  (score (make-connections (parse input) 1000)))

(defun make-all-the-connections (list)
  (loop for (a b) in (distance-pairs list)
        for circuits = (connect a b (or circuits (mapcar #'list list)))
        when (null (rest circuits))
          do (return (* (first a) (first b)))))

(defun part2 (input)
  (make-all-the-connections (parse input)))
