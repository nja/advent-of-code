;;;; day05.lisp

(in-package :aoc2023.day05)

(defun seeds (input)
  (mapcar #'parse-integer (rest (str:split " " (first (aoc:sections input))))))

(defun rules (input)
  (mapcar (a:compose (a:curry #'mapcar (a:compose (a:curry #'mapcar #'parse-integer)
                                                  (a:curry #'str:split " ")))
                     #'rest
                     #'aoc:lines)
          (rest (aoc:sections input))))

(defun map-number (rule number)
  (destructuring-bind (dst src range) rule
    (when (< (1- src) number (+ src range))
      (+ dst (- number src)))))

(defun apply-rules (numbers rules)
  (mapcar (lambda (n) (or (some (a:rcurry #'map-number n) rules) n))
          numbers))

(defun part1 (input)
  (reduce #'min (reduce #'apply-rules (rules input) :initial-value (seeds input))))

(defun parse-seed-runs (input)
  (loop for (a b) on (seeds input) by #'cddr
        collect (list a (+ a b -1))))

(defun apply-run-range (range run)
  (destructuring-bind (dst src range) range
    (let ((start src) (end (+ src range -1)))
      (destructuring-bind (lo hi) run
        (values (when (and (<= start hi) (<= lo end))
                  (let ((left (max start lo))
                        (right (min end hi)))
                    (list (+ dst (- left start))
                          (+ dst (- right start)))))
                (remove nil (list (when (< lo start)
                                    (list lo (min hi (1- start))))
                                  (when (< end hi)
                                    (list (max lo (1+ end)) hi)))))))))

(defun apply-run-map (runs map)
  (labels ((recur (runs map mapped)
             (if (null map)
                 (append mapped runs)
                 (recur (mapcan (lambda (run)
                                  (multiple-value-bind (m u)
                                      (apply-run-range (car map) run)
                                    (when m (push m mapped))
                                    u))
                                runs)
                        (cdr map)
                        mapped))))
    (recur runs map nil)))

;; (defun apply-run-maps (maps runs)
;;   (reduce #'apply-run-map maps :initial-value runs))

(defun part2 (input)
  (reduce #'min (mapcar #'car (reduce #'apply-run-map (rules input) :initial-value (parse-seed-runs input)))))

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