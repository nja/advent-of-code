;;;; day19.lisp

(in-package :aoc2019.day19)

(defun parse (input)
  (adjust-array (map 'vector #'parse-integer (str:split "," input)) #x200))

(defun run (memory &rest input)
  (let ((ip 0) (base 0) output)
    (macrolet ((ref (i) `(aref memory ,i))
               (rel (offset) `(aref memory (+ ip ,offset)))
               (bas (offset) `(aref memory (+ base ,offset))))
      (symbol-macrolet ((opcode (rel 0))
                        (ia (rel 1))
                        (ib (rel 2))
                        (ic (rel 3)))
        (symbol-macrolet ((pa (ref ia)) (ba (bas ia))
                          (pb (ref ib)) (bb (bas ib))
                          (pc (ref ic)) (bc (bas ic)))
          (labels ((mode (i) (mod (truncate opcode (expt 10 (1+ i))) 10))
                   (a () (ecase (mode 1) (0 pa) (1 ia) (2 ba)))
                   (b () (ecase (mode 2) (0 pb) (1 ib) (2 bb)))
                   (sa (x) (ecase (mode 1) (0 (setf pa x)) (2 (setf ba x))))
                   (sc (x) (ecase (mode 3) (0 (setf pc x)) (2 (setf bc x))))
                   (jmp (x) (setf ip x) 0))
            (loop do (incf ip (ecase (mod opcode 100)
                                (1 (sc (+ (a) (b))) 4)
                                (2 (sc (* (a) (b))) 4)
                                (3 (sa (pop input)) 2)
                                (4 (push (a) output) 2)
                                (5 (if (not (zerop (a))) (jmp (b)) 3))
                                (6 (if (zerop (a)) (jmp (b)) 3))
                                (7 (sc (if (< (a) (b)) 1 0)) 4)
                                (8 (sc (if (= (a) (b)) 1 0)) 4)
                                (9 (incf base (a)) 2)
                                (99 (return output)))))))))))

(defun affected? (memory x y)
  (plusp (first (run (copy-seq memory) x y))))

(defun count-affected (memory n)
  (loop for x below n
        sum (loop for y below n
                  count (affected? memory x y))))

(defun part1 (input)
  (count-affected (parse input) 50))

(defun first-corner (memory y)
  (loop for x from (truncate y 4)
        repeat y
        when (affected? memory x y)
          return (list x y)))

(defun next-corner (memory x y)
  (loop with ny = (1+ y)
        for nx from x
        when (affected? memory nx ny)
          return (list nx ny)))

(defun corners (s x y)
  (decf s)
  (list (list x (- y s))
        (list (+ x s) (- y s))))

(defun fits? (memory corners)
  (every (lambda (corner) (apply #'affected? memory corner))
         corners))

(defun closest-fitting-corner (memory size)
  (loop for corner = (first-corner memory size)
          then (apply #'next-corner memory corner)
        for corners = (apply #'corners size corner)
        when (fits? memory corners)
          return (first corners)))

(defun part2 (input)
  (destructuring-bind (x y) (closest-fitting-corner (parse input) 100)
    (+ (* x 10000) y)))
