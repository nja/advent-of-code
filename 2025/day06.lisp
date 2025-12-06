;;;; day06.lisp

(in-package :aoc2025.day06)

(defun parse (input)
  (reverse (mapcar #'aoc:read-as-list (aoc:lines input))))

(defun calc (lists)
  (let ((ops (first lists)))
    (reduce (lambda (x y)
              (loop for op in ops
                    for a in x
                    for b in y
                    collect (funcall op a b)))
            (rest lists))))

(defun part1 (input)
  (reduce #'+ (calc (parse input))))

(defun problems (input)
  (mapcar (lambda (x) (cons (op x) (columns x)))
          (collect input)))

(defun columns (strings)
  (mapcar (lambda (x) (read-from-string (coerce (butlast x) 'string)))
          (loop for i below (length (first strings))
                collect (mapcar (lambda (s) (aref s i)) strings))))

(defun op (strings)
  (read-from-string (car (last strings))))

(defun collect (input)
  (let ((lines (aoc:lines input))
        (starts (cons 0 (mapcar #'1+ (cuts input))))
        (ends (append (cuts input) (list nil))))
    (mapcar (lambda (start end)
              (mapcar (lambda (line)
                        (subseq line start end))
                      lines))
            starts
            ends)))

(defun cuts (input)
  (let ((lines (aoc:lines input)))
    (loop for i from 0 below (length (first lines))
          when (every (lambda (line) (char= #\Space (aref line i))) lines)
            collect i)))

(defun calc* (list)
  (reduce (symbol-function (first list)) (rest list)))

(defun part2 (input)
  (reduce #'+ (mapcar #'calc* (problems input))))
