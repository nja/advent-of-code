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
    (let ((first src)
          (last (+ src len -1)))
      (destructuring-bind (lo hi) range
        (values (when (and (<= first hi) (<= lo last))
                  (let ((left (max first lo))
                        (right (min last hi)))
                    (list (+ dst (- left first))
                          (+ dst (- right first)))))
                (remove nil (list (when (< lo first)
                                    (list lo (min hi (1- first))))
                                  (when (< last hi)
                                    (list (max lo (1+ last)) hi)))))))))

(defun apply-range-rules (ranges rules)
  (labels ((recur (ranges rules mapped)
             (if (null rules)
                 (append mapped ranges)
                 (recur (mapcan (lambda (run)
                                  (multiple-value-bind (m u)
                                      (map-range (car rules) run)
                                    (when m (push m mapped))
                                    u))
                                ranges)
                        (cdr rules)
                        mapped))))
    (recur ranges rules nil)))

(defun part2 (input)
  (reduce #'min (mapcar #'car (reduce #'apply-range-rules (rules input) :initial-value (ranges input)))))
