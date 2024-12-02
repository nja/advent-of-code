;;;; day15.lisp

(in-package :aoc2019.day15)

(defun parse (input)
  (adjust-array (map 'vector #'parse-integer (str:split "," input)) 2048))

(defun run (memory input)
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
                                (3 (sa input) 2)
                                (4 (setf output (a)) 2)
                                (5 (if (not (zerop (a))) (jmp (b)) 3))
                                (6 (if (zerop (a)) (jmp (b)) 3))
                                (7 (sc (if (< (a) (b)) 1 0)) 4)
                                (8 (sc (if (= (a) (b)) 1 0)) 4)
                                (9 (incf base (a)) 2)
                                (99 (return output))))
                  when output return it)))))))

(defun neighbours ()
  (let (next-to-oxygen)
    (lambda (pos)
      (if (eq pos 'oxygen)
          next-to-oxygen
          (loop for mem = (copy-seq pos)
                for c from 1 to 4
                for status = (run mem c)
                when (eq 1 status)
                  collect mem
                when (eq 2 status)
                  do (push pos next-to-oxygen)
                  and collect 'oxygen)))))

(defun fewest-steps (memory)
  (dijkstra:distance (dijkstra:search* memory (neighbours) :goal 'oxygen)))

(defun part1 (input)
  (fewest-steps (parse input)))

(defun how-many-minutes-to-fill-with-oxygen (memory)
  (let ((neighbours (neighbours)))
    (dijkstra:search* memory neighbours)
    (reduce #'max (mapcar #'dijkstra:distance (dijkstra:search* 'oxygen neighbours)))))

(defun part2 (input)
  ;; The repair droid can backtrack one step further than a map reveals?
  (1- (how-many-minutes-to-fill-with-oxygen (parse input))))
