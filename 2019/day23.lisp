;;;; day23.lisp

(in-package :aoc2019.day23)

(defun parse (input)
  (adjust-array (map 'vector #'parse-integer (str:split "," input)) #x1000))

(defun queues (n)
  (coerce (loop repeat n collect (q:make-queue :simple-queue)) 'vector))

(defun receive (queues address)
  (or (q:qpop (aref queues address)) -1))

(defparameter *nat* nil)

(defun send (queues address packet)
  (cond ((eql address #xff)
         (setf *nat* packet))
        (t
         (dolist (value packet)
           (q:qpush (aref queues address) value)))))

(defun boot (memory queues address)
  (let ((ip 0)
        (base 0)
        output)
    (lambda ()
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
              (incf ip (ecase (mod opcode 100)
                         (1 (sc (+ (a) (b))) 4)
                         (2 (sc (* (a) (b))) 4)
                         (3 (sa (receive queues address)) 2)
                         (4 (push (a) output) 2)
                         (5 (if (not (zerop (a))) (jmp (b)) 3))
                         (6 (if (zerop (a)) (jmp (b)) 3))
                         (7 (sc (if (< (a) (b)) 1 0)) 4)
                         (8 (sc (if (= (a) (b)) 1 0)) 4)
                         (9 (incf base (a)) 2)
                         (99 0)))
              (when (= 3 (length output))
                (destructuring-bind (y x dst) output
                  (setf output nil)
                  (send queues dst (list x y)))))))))))

(defun network (memory queues)
  (loop for address below (length queues)
        do (send queues address (list address))
        collect (boot (copy-seq memory) queues address)))

(defun tick (computers)
  (mapc #'funcall computers))

(defun part1 (input)
  (loop with *nat*
        with computers = (network (parse input) (queues 50))
        do (tick computers)
        when *nat* return (second *nat*)))

(defun idle? (queues)
  (notany (lambda (q) (plusp (q:qsize q))) queues))

(defun part2 (input)
  (loop with *nat*
        with queues = (queues 50)
        with computers = (network (parse input) queues)
        with delivered-ys
        do (tick computers)
        when (and *nat* (idle? queues))
          do (send queues 0 *nat*)
             (push (second *nat*) delivered-ys)
             (setf *nat* nil)
        when (and delivered-ys (eql (first delivered-ys) (second delivered-ys)))
          return (first delivered-ys) ))
