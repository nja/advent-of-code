;;;; day05.lisp

(in-package :aoc2023.day05)

(defun parse-seeds (input)
  (mapcar #'parse-integer (rest (str:split " " (first (aoc:sections input))))))

(defun parse-maps (input)
  (mapcar (a:compose (lambda (lines)
                       (mapcar (a:compose (a:curry #'mapcar #'parse-integer)
                                          (a:curry #'str:split " "))
                               lines))
                     #'rest
                     #'aoc:lines)
          (rest (aoc:sections input))))

(defun apply-seed-range (range number)
  (destructuring-bind (dst src range) range
    (when (< (1- src) number (+ src range))
      (+ dst (- number src)))))

(defun apply-seed-map (numbers map)
  (mapcar (lambda (n)
            (or (loop for range in map
                      when (apply-seed-range range n)
                        return it)
                n))
          numbers))

(defun apply-seed-maps (maps numbers)
  (reduce #'apply-seed-map maps :initial-value numbers))

(defun part1 (input)
  (reduce #'min (apply-seed-maps (parse-maps input) (parse-seeds input))))

(defun parse-seed-runs (input)
  (loop for (a b) on (parse-seeds input) by #'cddr
        collect (list a (+ a b -1))))

;; (defun apply-run-range (range run)
;;   (destructuring-bind (dst src range) range
;;     (let ((start src) (end (+ src range -1)))
;;       (destructuring-bind (lo hi) run
;;         (values (when (and (<= start hi) (<= lo end))
;;                   (let ((left (max start lo))
;;                         (right (min end hi)))
;;                     (list (+ dst (- left start))
;;                           (+ dst (- right start)))))
;;                 (remove nil (list (when (< lo start)
;;                                     (list lo (min hi (1- start))))
;;                                   (when (< end hi)
;;                                     (list (max lo (1+ end)) hi)))))))))

;; (defun apply-run-map (runs map)
;;   (labels ((recur (mapped unmapped ranges)
;;              (if (or (null ranges) (null unmapped))
;;                  (append mapped unmapped)
;;                  (let ((next
;;                          (loop for run in unmapped
;;                                for (m u) = (multiple-value-list
;;                                             (apply-run-range (car ranges) run))
;;                                when m
;;                                  do (push mapped m)
;;                                append u
;;                                do (break))))
;;                    (recur mapped next (cdr ranges))))))
;;     (recur nil runs map)))

(defun apply-run-map (runs map)
  (let (mapped)
    (labels ((recur (runs map)
               (if (null map)
                   (append mapped runs)
                   (let ((next
                           (loop for run in runs
                                 for (m u) = (multiple-value-list
                                              (apply-run-range (car map) run))
                                 when m
                                   do (push m mapped)
                                 append u)))
                     (recur next (cdr map))))))
      (recur runs map))))

;; (defun apply-run-map (runs map)
;;   (let* (mapped (unmapped (reduce (lambda (runs range)
;;                                     (break)
;;                                     (multiple-value-bind (m u) (apply-run-range range run)
;;                                       (push m mapped)
;;                                       u))
;;                                   map
;;                                   :initial-value runs)))
;;     (append unmapped mapped)))

(defun apply-run-maps (maps runs)
  (reduce #'apply-run-map maps :initial-value runs))

(defun part2 (input)
  (reduce #'min (mapcar #'car (apply-run-maps (parse-maps input) (parse-seed-runs input)))))

(defparameter *test*
  "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4")