;;;; day13.lisp

(in-package :aoc2019.day13)

(defun parse (input)
  (adjust-array (map 'vector #'parse-integer (str:split "," input)) #x1000))

(defun run/c (memory)
  (let ((ip 0) (base 0) output)
    (labels ((run ()
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
                              (aa () (ecase (mode 1) (0 ia) (2 (+ ia base))))
                              (sc (x) (ecase (mode 3) (0 (setf pc x)) (2 (setf bc x))))
                              (jmp (x) (setf ip x) 0))
                       (loop with cont
                             do (incf ip (ecase (mod opcode 100)
                                           (1 (sc (+ (a) (b))) 4)
                                           (2 (sc (* (a) (b))) 4)
                                           (3 (setf cont (let ((address (aa)))
                                                           (lambda (x)
                                                             (setf (ref address) x)
                                                             (run))))
                                            2)
                                           (4 (push (a) output) 2)
                                           (5 (if (not (zerop (a))) (jmp (b)) 3))
                                           (6 (if (zerop (a)) (jmp (b)) 3))
                                           (7 (sc (if (< (a) (b)) 1 0)) 4)
                                           (8 (sc (if (= (a) (b)) 1 0)) 4)
                                           (9 (incf base (a)) 2)
                                           (99 (return (values nil output)))))
                             when cont return (values cont output))))))))
      (run))))

(defun bounds (instructions)
  (loop for (x y) on instructions by #'cdddr
        maximize x into max-x
        maximize y into max-y
        finally (return (list (1+ max-y) (1+ max-x)))))

(defun draw (screen instructions)
  (loop with score
        for (x y tile) on instructions by #'cdddr
        if (minusp x)
          do (setf score tile)
        else
          do (setf (aref screen y x) (aref " #x=o" tile))
        finally (return score)))

(defun count-tiles (tile screen)
  (loop for i below (array-total-size screen)
        count (eql tile (row-major-aref screen i))))

(defun part1 (input)
  (let* ((instructions (reverse (nth-value 1 (run/c (parse input)))))
         (screen (make-array (bounds instructions))))
    (draw screen instructions)
    (count-tiles #\x screen)))

(defun input (screen)
  (loop with paddle and ball
        for y below (array-dimension screen 0)
        do (loop for x below (array-dimension screen 1)
                 for tile = (aref screen y x)
                 do (case tile
                      (#\= (setf paddle x))
                      (#\o (setf ball x))))
        finally (return (signum (- ball paddle)))))

(defun game (memory &optional print)
  (setf (aref memory 0) 2)
  (loop for (cont output) = (multiple-value-list (run/c memory))
          then (multiple-value-list (funcall cont (input screen)))
        for instructions = (reverse output) then (cdr (reverse output))
        for screen = (make-array (bounds instructions)) then screen
        for score = (or (draw screen instructions) score)
        do (rplacd output nil)
        when print
          do (terpri)
             (aoc:print-array screen)
             (print score)
             (sleep 1/60)
        while cont
        finally (return score)))

(defun part2 (input)
  (game (parse input)))
