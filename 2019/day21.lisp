;;;; day21.lisp

(in-package :aoc2019.day21)

(defun parse (input)
  (adjust-array (map 'vector #'parse-integer (str:split "," input)) #x1000))

(defun run (memory &optional input)
  (let ((ip 0)
        (base 0)
        (output (make-array 1024 :element-type 'character :adjustable t :fill-pointer 0))
        status)
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
                                (3 (sa (prog1 (char-code (vector-pop input))
                                         (when (zerop (fill-pointer input))
                                           (setf (fill-pointer output) 0)))) 2)
                                (4 (if (< (a) #x100)
                                       (vector-push-extend (code-char (a)) output)
                                       (setf status (a))) 2)
                                (5 (if (not (zerop (a))) (jmp (b)) 3))
                                (6 (if (zerop (a)) (jmp (b)) 3))
                                (7 (sc (if (< (a) (b)) 1 0)) 4)
                                (8 (sc (if (= (a) (b)) 1 0)) 4)
                                (9 (incf base (a)) 2)
                                (99 (return (values status output))))))))))))

(defun input (string)
  (let ((input (make-array (length string) :fill-pointer t :initial-contents string)))
    (nreverse input)))

(defun part1 (input)
  (run (parse input) (input
"NOT C J
AND D J
NOT A T
OR T J
WALK
")))

(defun memory () (parse (aoc:input)))

(defparameter *test*
)

(defun test ()
  (format t "~a" *test*)
  (run (memory) (input *test*)))

;;;@
;;;#####
;;; ABCD