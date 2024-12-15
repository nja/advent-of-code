;;;; day11.lisp

(in-package :aoc2024.day11)

(defun parse (input)
  (apply #'list* (aoc:read-integers input)))

(defun rule (stone)
  (cond ((zerop stone)
         1)
        ((evenp (digits stone))
         (multiple-value-call #'cons (halves stone)))
        (t
         (* stone 2024))))

(defun digits (n)
  (if (zerop n)
      1
      (1+ (truncate (log n 10)))))

(defun halves (stone)
  (truncate stone (expt 10 (/ (digits stone) 2))))

(defun blink (stones)
  (if (consp stones)
      (cons (blink (car stones))
            (blink (cdr stones)))
      (rule stones)))

(defun blinks (stones n)
  (dotimes (i n stones)
    (setf stones (blink stones))))

(defun part1 (input)
  (length (a:flatten (blinks (parse input) 25))))

(defparameter *blinks* nil)

(defun blink-2 (stone n)
  (or (gethash (cons stone n) *blinks*)
      (setf (gethash (cons stone n) *blinks*)
            (cond ((zerop n)
                   1)
                  ((zerop stone)
                   (blink-2 1 (1- n)))
                  ((evenp (digits stone))
                   (multiple-value-bind (a b) (halves stone)
                     (+ (blink-2 a (1- n))
                        (blink-2 b (1- n)))))
                  (t
                   (blink-2 (* stone 2024) (1- n)))))))

(defun blinks-2 (stones n)
  (let ((*blinks* (make-hash-table :test 'equal)))
    (reduce #'+ (mapcar (a:rcurry #'blink-2 n) stones))))

(defun part2 (input)
  (blinks-2 (aoc:read-integers input) 75))
