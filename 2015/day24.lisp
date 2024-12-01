;;;; day24.lisp

(in-package :aoc2015.day24)

(defun parse (input)
  (mapcar #'parse-integer (aoc:lines input)))

(defun configurations (packages groups)
  (let ((sum (truncate (reduce #'+ packages) groups)))
    (labels ((sum? (packages sum)
               (eql sum (reduce #'+ packages)))
             (rec (packages groups)
               (if (eql 1 groups)
                   (list (when (sum? packages sum)
                           packages))
                   (loop with configurations
                         for i from 1 below (length packages)
                         do (a:map-combinations
                             (lambda (combination)
                               (when (sum? combination sum)
                                 (a:when-let ((subs (rec
                                                     (set-difference packages combination)
                                                     (1- groups))))
                                   (push (cons combination subs) configurations))))
                             packages :length i)
                         when configurations return it))))
      (rec packages groups))))

(defun quantum-entanglement (configuration)
  (reduce #'* (car configuration)))

(defun part1 (input)
  (reduce #'min (configurations (parse input) 3) :key #'quantum-entanglement))

(defun part2 (input)
  (reduce #'min (configurations (parse input) 4) :key #'quantum-entanglement))
