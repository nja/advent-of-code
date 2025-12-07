;;;; day06.lisp

(in-package :aoc2025.day06)

(defun parse (input)
  (transpose (reverse (mapcar #'aoc:read-as-list (aoc:lines input)))))

(defun transpose (lists)
  (apply #'mapcar #'list lists))

(defun calc (list)
  (reduce (symbol-function (first list)) (rest list)))

(defun part1 (input)
  (reduce #'+ (mapcar #'calc (parse input))))

(defun problems (input)
  (mapcar (lambda (x) (cons (op x) (columns x)))
          (collect input)))

(defun columns (strings)
  (mapcar (lambda (x) (read-from-string (coerce (butlast x) 'string)))
          (loop for i below (length (first strings))
                collect (mapcar (lambda (s) (aref s i)) strings))))

(defun op (strings)
  (read-from-string (a:lastcar strings)))

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

(defun part2 (input)
  (reduce #'+ (mapcar #'calc (problems input))))
