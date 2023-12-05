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

(defun ranges (input)
  (loop for (a b) on (seeds input) by #'cddr
        collect (list a (+ a b -1))))

(defun map-range (rule range)
  (destructuring-bind (dst src len) rule
    (flet ((ofs (x) (+ dst (- x src))))
      (let ((end (+ src len -1)))
        (destructuring-bind (lo hi) range
          (values (unless (or (< hi src) (< end lo))
                    (list (ofs (max src lo)) (ofs (min end hi))))
                  (remove nil (list (when (< lo src)
                                      (list lo (min hi (1- src))))
                                    (when (< end hi)
                                      (list (max lo (1+ end)) hi))))))))))

(defun apply-range-rules (ranges rules)
  (labels ((recur (ranges rules mapped)
             (if (null rules)
                 (append mapped ranges)
                 (recur (mapcan (lambda (range)
                                  (multiple-value-bind (m u)
                                      (map-range (car rules) range)
                                    (when m (push m mapped))
                                    u))
                                ranges)
                        (cdr rules)
                        mapped))))
    (recur ranges rules nil)))

(defun part2 (input)
  (reduce #'min (mapcar #'car (reduce #'apply-range-rules (rules input) :initial-value (ranges input)))))
