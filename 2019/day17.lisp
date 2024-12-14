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

(defclass robot ()
  ((x :initarg :x)
   (y :initarg :y)
   (dx :initarg :dx)
   (dy :initarg :dy)
   (map :initarg :map)))

(defun robot (map)
  (destructuring-bind (x y) (start-position map)
    (make-instance 'robot :x x :y y
                          :dx 0 :dy -1
                          :map map)))

(defun done? (robot)
  (with-slots (x y) robot
    (and robot (eql x 34) (eql y 26))))

(defun start-position (map)
  (loop for row below (array-dimension map 0)
        do (loop for col below (array-dimension map 1)
                 when (eql #\^ (aref map row col))
                   do (return-from start-position (list col row)))))

(defun act (robot m)
  (cond ((integerp m)
         (forward robot m))
        ((eql 'R m)
         (right robot))
        ((eql 'L m)
         (left robot))
        ((eql 'LL m)
         (left robot)
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
         (find (aref map y x) "^#"))))

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
    (and (loop for r in main
               always (routine (case r (a a) (b b) (c c))))
         robot)))

(defun print-robot (robot)
  (with-slots (x y dx dy map) robot
    (let ((map (a:copy-array map)))
      (setf (aref map y x) 'R)
      (aoc:print-array map))
    (format t "~& X: ~2d  Y: ~2d~%dX: ~2d dY: ~2d~%" x y dx dy)))

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
    (flet ((buffer (inputs)
             (let ((i (fill-pointer buffer)))
               (format buffer "~{~a~^,~}~%" (mapcar (lambda (x)
                                                      (case x
                                                        (LL "L,L")
                                                        (t x)))
                                                    inputs))
               (< (- (fill-pointer buffer) i) 22))))
      (when (and (buffer main)
                 (buffer a)
                 (buffer b)
                 (buffer c)
                 (buffer '(n)))
        (nreverse buffer)))))

(defun crash? (output)
  (find #\X output))

(defun neighbours (map status-and-logic)
  (destructuring-bind (status (main a b c)) status-and-logic
    (declare (ignore status))
    (when (and (input main a b c)
               (trial (robot map) main a b c))
      (extend main a b c))))

(defun extend (main a b c)
  (let (results)
    (flet ((collect (main a b c)
             (push (list main a b c) results)))
      (dolist (main (extend-main main))
        (collect main a b c))
      (case (car (last main))
        (a (dolist (a (extend-movement a))
             (collect main a b c)))
        (b (dolist (b (extend-movement b))
             (collect main a b c)))
        (c (dolist (c (extend-movement c))
             (collect main a b c)))))
    results))

(defun extend-movement (moves)
  (if (null moves)
      '((R) (L) (LL) (1))
      (let ((last (car (last moves))))
        (if (integerp last)
            (list (append moves '(R))
                  (append moves '(L))
                  (append moves '(LL))
                  (append (butlast moves) (list (1+ last))))
            (list (append moves '(1)))))))

(defun extend-main (routines)
  (loop for r in '((a) (b) (c))
        collect (append routines r)))

(defun part2 (input)
  (let ((*memory* (wake-up (parse input))))
    (dijkstra:item (dijkstra:search* '(nil nil ((A) () (R) (L))) #'neighbours
                                     :donep (lambda (x) (and (numberp (first x))
                                                             (> (first x) 30000)))))))

;;; (runf (input '(a b c) '(R 8 R 8) '(r 4 r 4) '(1 2 3 4 5 6 7 8 9) 'y))