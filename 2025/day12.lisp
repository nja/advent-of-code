;;;; day12.lisp

(in-package :aoc2025.day12)

(defun parse-shapes (input)
  (mapcar (lambda (section) (aoc:to-array (subseq section (1+ (position #\Newline section)))))
          (butlast (aoc:sections input))))

(defun parse-regions (input)
  (mapcar #'aoc:read-as-list (aoc:lines (a:lastcar (aoc:sections (aoc:tr "x:" "  " input))))))

(defun make-region-array (region)
  (make-array (reverse (subseq region 0 2))))

(defun shape-counts (region)
  (subseq region 2))

(defun shape-size (shape)
  (loop for i below (array-total-size shape)
        count (char= #\# (row-major-aref shape i))))

(defun region-total-size (region)
  (* (first region) (second region)))

(defun shape-minimum-size (shape count)
  (* (shape-size shape) count))

(defun region-too-small? (shapes region)
  (< (region-total-size region) (reduce #'+ (mapcar #'shape-minimum-size shapes (shape-counts region)))))

(defun part1 (input)
  (count-if-not (a:curry #'region-too-small? (parse-shapes input))
                (parse-regions input)))
