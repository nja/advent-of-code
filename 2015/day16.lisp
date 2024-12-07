;;;; day16.lisp

(in-package :aoc2015.day16)

(defun parse (input)
  (mapcar (lambda (line)
            (aoc:read-as-list (aoc:tr ":," "  " line)))
          (aoc:lines input)))

(defparameter *tape* (parse
"children: 3
cats: 7
samoyeds: 2
pomeranians: 3
akitas: 0
vizslas: 0
goldfish: 5
trees: 3
cars: 2
perfumes: 1"))

(defun eliminate (sues thing)
  (destructuring-bind (indicator count) thing
    (remove-if-not (lambda (sue)
                     (eql count (getf sue indicator count)))
                   sues)))

(defun gift-giver (sues eliminate)
  (car (reduce eliminate *tape* :initial-value sues)))

(defun part1 (input)
  (getf (gift-giver (parse input) #'eliminate) 'sue))

(defun eliminate* (sues thing)
  (destructuring-bind (indicator count) thing
    (remove-if-not
     (lambda (sue)
       (or (null (getf sue indicator))
           (funcall (case indicator
                      ((cats trees) #'>)
                      ((pomeranians goldfish) #'<)
                      (t #'eql))
                    (getf sue indicator) count)))
     sues)))

(defun part2 (input)
  (getf (gift-giver (parse input) #'eliminate*) 'sue))
