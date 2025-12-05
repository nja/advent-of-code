;;;; day05.lisp

(in-package :aoc2025.day05)

(defun parse (input)
  (destructuring-bind (ranges fresh) (aoc:sections input)
    (values (mapcar (lambda (line) (aoc:read-as-list (aoc:tr "-" " " line))) (aoc:lines ranges))
            (aoc:read-as-list fresh))))

(defun overlaps? (a b)
  (destructuring-bind ((a1 a2) (b1 b2)) (list a b)
    (not (or (< a2 b1) (< b2 a1)))))

(defun join (a b)
  (destructuring-bind ((a1 a2) (b1 b2)) (list a b)
    (list (min a1 b1) (max a2 b2))))

(defun unique-ranges (ranges)
  (when ranges
    (loop with first = (first ranges)
          for x in (unique-ranges (rest ranges))
          if (overlaps? first x)
            do (setf first (join first x))
          else
            collect x into unique
          finally (return (cons first unique)))))

(defun count-fresh (range fresh)
  (destructuring-bind (min max) range
    (count-if (lambda (x) (<= min x max)) fresh)))

(defun part1 (input)
  (multiple-value-bind (ranges fresh) (parse input)
    (reduce #'+ (mapcar (lambda (r) (count-fresh r fresh))
                        (unique-ranges ranges)))))

(defun count-fresh* (range)
  (1+ (- (second range) (first range))))

(defun part2 (input)
  (reduce #'+ (mapcar #'count-fresh* (unique-ranges (parse input)))))
