;;;; day02.lisp

(in-package :aoc2025.day02)

(defun ranges (input)
  (mapcar #'aoc:read-as-list (mapcar (a:curry #'aoc:tr "-" " ") (str:split #\, input))))

(defun invalidp (n)
  (let ((s (format nil "~d" n)))
    (and (evenp (length s))
         (repeats s (/ (length s) 2)))))

(defun repeats (string len)
  (labels ((rec (i)
             (if (= i (length string))
                 t
                 (and (string= string string :end1 len :start2 i :end2 (+ i len))
                      (rec (+ i len))))))
    (when (integerp (/ (length string) len))
      (rec 0))))

(defun sum (first last predicate)
  (loop for x from first to last
        when (funcall predicate x)
          sum x))

(defun part1 (input)
  (loop for (first last) in (ranges input)
        sum (sum first last #'invalidp)))

(defun invalidp* (n)
  (let ((s (format nil "~d" n)))
    (loop for i from 1 upto (truncate (length s) 2)
            thereis (repeats s i))))

(defun part2 (input)
  (loop for (first last) in (ranges input)
        sum (sum first last #'invalidp*)))
