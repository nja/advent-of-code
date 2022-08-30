;;;; day03.lisp

(in-package #:aoc2021.day03)

(defun bits (line)
  (map 'list #'digit-char-p line))

(defun bitses (lines)
  (mapcar #'bits lines))

(defun positions (bitses)
  (apply #'mapcar #'list bitses))

(defun gamma (bitses)
  (mapcar #'most-common (positions bitses)))

(defun flip (bits)
  (mapcar (a:curry #'logxor 1) bits))

(defun epsilon (gamma)
  (flip gamma))

(defun bits-value (bits)
  (loop for b in (reverse bits)
        for p from 0
        sum (ash b p)))

(defun part1 (input)
  (let ((gamma (gamma (bitses (aoc:lines input)))))
    (* (bits-value gamma)
       (bits-value (epsilon gamma)))))

(defun oxy-filter (firsts)
  (let ((bit (most-common firsts 1)))
    (lambda (bits)
      (eql (first bits) bit))))

(defun co2-filter (firsts)
  (complement (oxy-filter firsts)))

(defun firsts (list)
  (mapcar #'first list))

(defun most-common (firsts &optional tie-break)
  (loop for n in firsts
        counting (eq n 1) into 1s
        counting (eq n 0) into 0s
        finally (return (cond ((> 1s 0s) 1)
                              ((> 0s 1s) 0)
                              (t tie-break)))))

(defun rating (filter bitses)
  (let ((filtered (remove-if-not (funcall filter (firsts bitses)) bitses)))
    (if (= 1 (length filtered))
        (first filtered)
        (cons (car (first filtered))
              (rating filter (mapcar #'cdr filtered))))))

(defun part2 (input)
  (let ((bitses (bitses (aoc:lines input))))
    (* (bits-value (rating #'oxy-filter bitses))
       (bits-value (rating #'co2-filter bitses)))))
