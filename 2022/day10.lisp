;;;; day10.lisp

(in-package #:aoc2022.day10)

(defun parse (input)
  (mapcar #'aoc:read-as-list (aoc:lines input)))

(defun interpret (instructions)
  (let ((registers (make-array (* 2 (length instructions)) :fill-pointer 0))
        (x 1))
    (flet ((tick () (vector-push x registers)))
      (tick)
      (loop for (op operand) in instructions
            do (case op
                 (noop (tick))
                 (addx (tick) (tick) (incf x operand)))))
    registers))

(defun part1 (input)
  (loop with signals = (interpret (parse input))
        for cycle from 20 by 40 upto 220
        sum (* cycle (aref signals cycle))))

(defun render (registers)
  (flet ((line (registers)
           (coerce (loop for x across registers
                         for pos from 0
                         collect (if (< (abs (- x pos)) 2) #\# #\.))
                   'string)))
    (loop for cycle from 1 by 40 below (length registers)
          collect (line (subseq registers cycle (+ cycle 40))))))

(defun part2 (input)
  (render (interpret (parse input))))
