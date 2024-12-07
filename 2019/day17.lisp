;;;; day17.lisp

(in-package :aoc2019.day17)

(defun parse (input)
  (adjust-array (map 'vector #'parse-integer (str:split "," input)) #x1000))

(defun run (memory &optional input)
  (let ((ip 0) (base 0) (output (make-array 1024 :element-type 'character :adjustable t :fill-pointer 0)))
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
                                (3 (sa (vector-pop input)) 2)
                                (4 (if (< (a) #x100)
                                       (vector-push-extend (code-char (a)) output)
                                       (setf output (a))) 2)
                                (5 (if (not (zerop (a))) (jmp (b)) 3))
                                (6 (if (zerop (a)) (jmp (b)) 3))
                                (7 (sc (if (< (a) (b)) 1 0)) 4)
                                (8 (sc (if (= (a) (b)) 1 0)) 4)
                                (9 (incf base (a)) 2)
                                (99 (return output)))))))))))

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

(defun wake-up (memory)
  (prog1 memory
    (setf (aref memory 0) 2)))

(defun input (main a b c)
  (let ((input (make-array #xff :element-type 'character :fill-pointer 0)))
    (format input "~@{~{~a~^,~}~^~%~}n" main a b c)
    (nreverse input)))

(defun join (list)
  (format nil "~{~a~^,~}" list))

(defun 2d-map (camera-output)
  (let* ((cols (reduce #'max (mapcar #'length (aoc:lines camera-output))))
         (rows (length (aoc:lines camera-output)))
         (map (make-array (list rows cols) :initial-element #\Space)))
    (loop with row = 0
          for col = 0 then (if (eql c #\Newline) 0 (1+ col))
          for c across camera-output
          do (cond ((eq c #\Newline)
                    (incf row))
                   (t
                    (setf (aref map row col) c)))
          finally (return map))))

(defun part2 (input)
  (wake-up (parse input)))