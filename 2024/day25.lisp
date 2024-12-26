;;;; day25.lisp

(in-package :aoc2024.day25)

(defun parse (input)
  (mapcar (lambda (x) (parse-integer x :radix 2))
          (mapcar (lambda (section) (remove #\Newline (aoc:tr "#." "10" section)))
                  (aoc:sections input))))

(defun overlap (a b)
  (plusp (logand a b)))

(defun count-unless-overlap (things)
  (let ((count 0))
    (a:map-combinations (lambda (p) (unless (apply #'overlap p) (incf count))) things :length 2)
    count))

(defun part1 (input)
  (count-unless-overlap (parse input)))
