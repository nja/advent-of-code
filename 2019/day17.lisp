;;;; day17.lisp

(in-package :aoc2019.day17)

(defun parse (input)
  (adjust-array (map 'vector #'parse-integer (str:split "," input)) #x1000))

(defun intersections (camera-output)
  (let ((lines (aoc:lines camera-output))
        (sum 0))
    (map nil (lambda (row a b c)
               (map nil (lambda (col &rest chars)
                          (when (every (a:curry #'eql #\#) chars)
                            (incf sum (* row col))))
                    (indexes a) a b c (subseq b 1)))
         (indexes lines 1)
         lines
         (rest lines)
         (rest (rest lines)))
    sum))

(defun indexes (sequence &optional (start 0))
  (map 'list (lambda (x) (declare (ignore x)) (prog1 start (incf start))) sequence))

(defun part1 (input)
  (intersections (run (parse input))))

(defparameter *memory* nil)
(defun memory () (a:copy-array *memory*))

(defun run (logic)
  (run* (memory) (apply #'input logic)))

(defun run* (memory &optional input)
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

(defun wake-up (memory)
  (prog1 memory
    (setf (aref memory 0) 2)))

(defun input (main a b c)
  (let ((buffer (make-array #xff :element-type 'character :fill-pointer 0)))
    (flet ((buffer (x)
             (let ((i (fill-pointer buffer)))
               (format buffer "~{~a~^,~}~%" x)
               (< (- (fill-pointer buffer) i) 22))))
      (when (and (buffer main)
                 (buffer a)
                 (buffer b)
                 (buffer c)
                 (buffer '(n)))
        (nreverse buffer)))))

(defun crash? (output)
  (find #\X output))

(defun neighbours (results-and-logic)
  (destructuring-bind (status output logic) results-and-logic
    (declare (ignore status))
    (when (not (crash? output))
      (mapcar (lambda (logic)
                (if (member nil logic)
                    (list nil nil logic)
                    (multiple-value-bind (status output) (run logic)
                      (list status output logic))))
              (extend logic)))))

(defun extend (logic)
  (let (results)
    (flet ((collect (main a b c)
             (push (list main a b c) results)))
      (destructuring-bind (main a b c) logic
        (dolist (main (extend-main main))
          (collect main a b c))
        (dolist (a (extend-movement a))
          (collect main a b c))
        (dolist (b (extend-movement b))
          (collect main a b c))
        (dolist (c (extend-movement c))
          (collect main a b c))))
    results))

(defun extend-movement (moves)
  (loop for m in '((r) (l) (1) (2) (3) (4) (5) (6) (7) (8) (9) (10))
        collect (append moves m)))

(defun extend-main (routines)
  (loop for r in '((a) (b) (c))
        collect (append routines r)))

(defun part2 (input)
  (wake-up (parse input)))

;;; (runf (input '(a b c) '(R 8 R 8) '(r 4 r 4) '(1 2 3 4 5 6 7 8 9) 'y))