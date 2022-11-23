;;;; day19.lisp

(in-package #:aoc2016.day19)

(defun elves (n)
  (let (head tail)
    (dotimes (i n head)
      (cond (tail (setf tail (cdr (rplacd tail (cons (1+ i) head)))))
            (t (setf head (cons (1+ i) nil)
                     tail (rplacd head head)))))))

(defun game (elves)
  (loop while (not (eq elves (cdr elves)))
        do (rplacd elves (cddr elves))
           (setf elves (cdr elves))
        finally (return (car elves))))

(defun part1 (input)
  (game (elves (parse-integer input))))

(defun game2 (elves n)
  (loop with across = (nthcdr (1- (truncate n 2)) elves)
        while (not (eq elves (cdr elves)))
        do (rplacd across (cddr across))
           (setf elves (cdr elves))
           (when (evenp (decf n))
             (setf across (cdr across)))
        finally (return (car elves))))

(defun part2 (input)
  (let ((n (parse-integer input)))
    (game2 (elves n) n)))
