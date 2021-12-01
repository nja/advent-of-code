(in-package #:aoc2021.day01)

;; (defparameter *lines* (aoc:lines (aoc:input-for 2018 25)))
;; (defparameter *ints* (mapcar #'parse-integer *lines*))

;; (defun part1 ()
;;   (loop for (p x) on *ints*
;;        when (and x p (> x p)) count it
;;          ))
;; ;;1374

;; (defun part2 ()
;;   (loop
;;         for (x y z) on *ints*
;;         for m = (when (and x y z)
;;                   (+ x y z))
;;         and pm = nil then m
;;         while (and x y z)
;;         when (and m pm (> m pm))
;;           count it))
;; ;;1418

(defparameter *test* (list 1 2 3 4 5 6 7))

(defun parse (input)
  (mapcar #'parse-integer (aoc:lines input)))

(defun predicate2int (p)
  (lambda (&rest args)
    (if (apply p args) 1 0)))

(defun offsets (n list)
  (loop repeat n
        for x on list
        collect x))

(defun count-increases (list)
  (reduce #'+ (apply #'mapcar (predicate2int #'<) (offsets 2 list))))

(defun part1 (input)
  (count-increases (parse input)))

(defun windows (n list)
  (apply #'mapcar #'list (offsets n list)))

(defun sums (windows)
  (mapcar (a:curry #'apply #'+) windows))

(defun part2 (input)
  (count-increases (sums (windows 3 (parse input)))))