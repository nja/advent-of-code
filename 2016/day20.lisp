;;;; day20.lisp

(in-package #:aoc2016.day20)

(defun parse-range (line)
  (mapcar #'parse-integer (ppcre:split "-" line)))

(defun parse (input)
  (sort (mapcar #'parse-range (aoc:lines input)) #'compare-range))

(defun compare-range (a b)
  (aoc:comparisons a b (<) car cadr))

(defun lowest-allowed (blacklist)
  (loop with i = 0
        for (min max) in blacklist
        when (<= min i max)
          do (setf i (1+ max))
        finally (return i)))

(defun part1 (input)
  (lowest-allowed (parse input)))

(defun difference (a b)
  (remove-if
   #'empty?
   (destructuring-bind (min-a max-a) a
     (destructuring-bind (min-b max-b) b
       (cond ((< max-b min-a) (list a))
             ((< max-a min-b) (list a))
             ((<= min-b min-a max-a max-b) nil)
             ((<= min-a min-b max-b max-a) (list (list min-a (1- min-b))
                                                 (list (1+ max-b) max-a)))
             ((<= min-b min-a max-b max-a) (list (list (1+ max-b) max-a)))
             ((<= min-a min-b max-a max-b) (list (list min-a (1- min-b)))))))))

(defun empty? (x)
  (< (second x) (first x)))

(defun apply-blacklist (ips blacklist)
  (reduce (lambda (x b) (mapcan (a:rcurry #'difference b) x))
          blacklist :initial-value ips))

(defun sum-ranges (ranges)
  (loop for (min max) in ranges
        sum (1+ (- max min))))

(defun part2 (input)
  (sum-ranges (apply-blacklist '((0 4294967295)) (parse input))))
