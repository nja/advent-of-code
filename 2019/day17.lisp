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
  (intersections (nth-value 1 (run (parse input)))))

(defun make-map (input)
  (aoc:to-array (nth-value 1 (run (parse input)))))

(defclass robot ()
  ((x :initarg :x)
   (y :initarg :y)
   (dx :initarg :dx)
   (dy :initarg :dy)
   (map :initarg :map)
   (status :initform 'fresh)))

(defun robot (map)
  (destructuring-bind (x y) (start-position map)
    (make-instance 'robot :x x :y y
                          :dx 0 :dy -1
                          :map map)))

(defun start-position (map)
  (loop for row below (array-dimension map 0)
        do (loop for col below (array-dimension map 1)
                 when (eql #\^ (aref map row col))
                   do (return-from start-position (list col row)))))

(defun can-move-forward? (robot)
  (with-slots (x y dx dy map) robot
    (walkable? map (+ x dx) (+ y dy))))

(defun can-turn-right? (robot)
  (with-slots (x y dx dy map) robot
    (walkable? map (- x dy) y)))

(defun can-turn-left? (robot)
  (with-slots (x y dx dy map) robot
    (walkable? map x (- y dx))))

(defun walkable? (map x y)
  (and (array-in-bounds-p map y x)
       (eql #\# (aref map y x))))

(defun act (robot m)
  (cond ((and (integerp m) (can-move-forward? robot))
         (forward robot m))
        ((and (eql 'R m) (not (can-move-forward? robot)))
         (right robot))
        ((and (eql 'L m) (not (can-move-forward? robot)))
         (left robot))))

(defun forward (robot n)
  (with-slots (x y dx dy) robot
    (loop repeat n
          always (can-move-forward? robot)
          do (incf x dx)
             (incf y dy))))

(defun safe? (robot)
  (with-slots (x y dx dy map status) robot
    (let ((safe? (and (array-in-bounds-p map y x)
                      (find (aref map y x) "^#"))))
      (cond ((and (not safe?) (eq status 'alive))
             (setf status nil))
            (safe?
             (setf status (list x y))))
      safe?)))

(defun right (robot)
  (with-slots (dx dy) robot
    (rotatef dx dy)
    (setf dx (- dx))
    (can-move-forward? robot)))

(defun left (robot)
  (with-slots (dx dy) robot
    (rotatef dx dy)
    (setf dy (- dy))
    (can-move-forward? robot)))

(defun routines-defined? (main a b c)
  (and (or a b c)
       (or a (not (member 'a main)))
       (or b (not (member 'b main)))
       (or c (not (member 'c main)))))

(defun trial (robot main a b c)
  (flet ((routine (r)
           (loop for a in r
                 always (act robot a))))
    (and (loop for r in main
               always (routine (case r (a a) (b b) (c c))))
         (with-slots (x y) robot
           (or (and (eql x 34) (eql y 26) 'done)
               (list y x))))))

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
               (format buffer "~{~a~^,~}~%" inputs)
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
  (assert (= 5 (length status-and-logic)))
  (destructuring-bind (main a b c) (rest status-and-logic)
    (loop for (main a b c) in (extend main a b c)
          for status = (and (routines-defined? main a b c)
                            (input main a b c)
                            (trial (robot map) main a b c))
          when status
            collect (list status main a b c))))

(defun extend (main a b c)
  (let (results)
    (flet ((collect (main a b c)
             (push (list main a b c) results)))
      (when (member 'a main)
        (dolist (a (extend-movement a))
          (collect main a b c)))
      (when (member 'b main)
        (dolist (b (extend-movement b))
          (collect main a b c)))
      (when (member 'c main)
        (dolist (c (extend-movement c))
          (collect main a b c)))
      (let ((mains (extend-main main)))
        (dolist (main mains)
          (collect main a b c)
          (when (member 'a main)
            (dolist (a (extend-movement a))
              (collect main a b c)))
          (when (member 'b main)
            (dolist (b (extend-movement b))
              (collect main a b c)))
          (when (member 'c main)
            (dolist (c (extend-movement c))
              (collect main a b c))))))
    results))

(defun extend-movement (moves)
  (let ((dirs '((L) (R)))
        (nums '((1) (2) (3) (4) (5) (6) (7) (8) (9) (10) (11) (12) (13) (14) (15))))
    (if (null moves)
        (loop for d in dirs
              append (loop for n in nums
                           collect (append d n)))
        (let ((last (car (last moves))))
          (if (integerp last)
              (loop for d in dirs
                    collect (append moves d))
              (loop for n in nums
                    collect (append moves n)))))))

(defun range (n)
  (loop for i from 1 repeat n collect (list i)))

(defun extend-main (routines)
  (loop for r in '((a) (b) (c))
        collect (append routines r)))

(defun score (n)
  (if (eq 'done (first n))
      0
      (* 2 (reduce #'+ (mapcar (a:compose #'abs #'-) (first n) '(26 34))))))

(defun mn (map p)
  (loop for d in '((1 0) (0 1) (-1 0) (0 -1))
        for n = (add p d)
        for (row col) = n
        when (and (array-in-bounds-p map row col)
                  (eql #\# (aref map row col)))
          collect n))

(defun add (a b)
  (mapcar #'+ a b))

(defun part2 (input)
  (let ((path (rest (astar::item (astar:search '((2 44) (a) nil nil nil)
                                               (a:curry #'neighbours (make-map input))
                                               (lambda (x) (eql 'done (car x)))
                                               :scoref #'score)))))
    (run (wake-up (parse input)) (apply #'input path))))
