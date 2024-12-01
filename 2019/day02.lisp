;;;; day02.lisp

(in-package :aoc2019.day02)

(defun parse (input)
  (map 'vector #'parse-integer (str:split "," input)))

(defun patch (memory pos &rest values)
  (let ((memory (a:copy-array memory)))
    (loop for i from pos
          for v in values
          do (setf (aref memory i) v)
          finally (return memory))))

(defun run (memory)
  (let ((ip 0))
    (macrolet ((ref (i) `(aref memory ,i))
               (rel (offset) `(aref memory (+ ip ,offset))))
      (symbol-macrolet ((opcode (rel 0))
                        (a (rel 1))
                        (b (rel 2))
                        (c (rel 3)))
        (symbol-macrolet ((*a (ref a))
                          (*b (ref b))
                          (*c (ref c)))
          (loop do (ecase opcode
                     (1 (setf *c (+ *a *b)))
                     (2 (setf *c (* *a *b)))
                     (99 (return (aref memory 0))))
                   (incf ip 4)))))))

(defun part1 (input)
  (run (patch (parse input) 1 12 2)))

(defun find-inputs (program output)
  (let ((range (loop for i to 99 collect i)))
    (a:map-product
     (lambda (noun verb)
       (when (eql output (run (patch program 1 noun verb)))
         (return-from find-inputs (list noun verb))))
     range range)))

(defun answer (noun verb)
  (+ (* 100 noun) verb))

(defun part2 (input)
  (apply #'answer (find-inputs (parse input) 19690720)))
