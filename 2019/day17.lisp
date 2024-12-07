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

(defun make-map (input)
  (aoc:to-array (nth-value 1 (run (parse input)))))

(defclass robot () (x y dx dy map))

(defun act (robot m)
  (cond ((integerp m)
         (forward robot m))
        ((eql 'R m)
         (right robot))
        ((eql 'L m)
         (left robot))))

(defun forward (robot n)
  (with-slots (x y dx dy) robot
    (loop repeat n
          do (incf x dx)
             (incf y dy)
          while (safe? robot)
          finally (return (safe? robot)))))

(defun safe? (robot)
  (with-slots (x y dx dy map) robot
    (and (array-in-bounds-p map y x)
         (eql #\# (aref map y x)))))

(defun right (robot)
  (with-slots (x y dx dy) robot
    (let ((ndx (- dy))
          (ndy dx))
      (setf dx ndx
            dy ndy))))

(defun left (robot)
  (with-slots (x y dx dy) robot
    (let ((ndx dy)
          (ndy (- dx)))
      (setf dx ndx
            dy ndy))))

(defun trial (robot main a b c)
  (flet ((routine (r)
           (loop for a in r
                 always (act robot a))))
    (loop for r in main
          always (routine (case r (a a) (b b) (c c))))))

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
    (a:when-let (input (apply #'input logic))
      (when (not (crash? output))
        (mapcar (lambda (logic)
                  (if (runnable? logic)
                      (progn (print logic)
                             (multiple-value-bind (status output) (run (memory) (a:copy-array input))
                               (list status output logic)))
                      (list nil nil logic)))
                (extend logic))))))

(defun runnable? (logic)
  (not (member nil logic)))

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
  (if (null moves)
      '((R) (L) (1))
      (loop for n in (turns moves)
            collect (append moves n))))

(defun turns (moves)
  (loop with r = 0 and l = 0 and i
        for x in moves
        do (case x
             (r (incf r) (setf l 0 i nil))
             (l (incf l) (setf r 0 i nil))
             (t (setf i x r 0 l 0)))
        finally (case x
                  (r (if (< r 2)
                         '((r) (1))
                         '((1))))
                  (l (if (< l 2)
                         '((l) (1))
                         '((1))))
                  (t '((1))))))

(defun extend-main (routines)
  (loop for r in '((a) (b) (c))
        collect (append routines r)))

(defun part2 (input)
  (let ((*memory* (wake-up (parse input))))
    (dijkstra:item (dijkstra:search* '(nil nil ((A) () (R) (L))) #'neighbours
                                     :donep (lambda (x) (and (numberp (first x))
                                                             (> (first x) 30000)))))))

;;; (runf (input '(a b c) '(R 8 R 8) '(r 4 r 4) '(1 2 3 4 5 6 7 8 9) 'y))