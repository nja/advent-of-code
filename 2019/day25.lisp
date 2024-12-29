;;;; day25.lisp

(in-package :aoc2019.day25)

(defun parse (input)
  (list (adjust-array (map 'vector #'parse-integer (str:split "," input)) #x2000) 0 0))

(defun run (state cmd)
  (destructuring-bind (memory ip base) state
    (setf memory (copy-seq memory))
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
            (loop with input = (input-stack cmd)
                  with output = (make-array #x100 :element-type 'character :adjustable t :fill-pointer 0)
                  do (incf ip (ecase (mod opcode 100)
                                (1 (sc (+ (a) (b))) 4)
                                (2 (sc (* (a) (b))) 4)
                                (3 (cond ((zerop (fill-pointer input))
                                          (setf input nil)
                                          0)
                                         (t
                                          (sa (char-code (vector-pop input)))
                                          2)))
                                (4 (vector-push-extend (code-char (a)) output) 2)
                                (5 (if (not (zerop (a))) (jmp (b)) 3))
                                (6 (if (zerop (a)) (jmp (b)) 3))
                                (7 (sc (if (< (a) (b)) 1 0)) 4)
                                (8 (sc (if (= (a) (b)) 1 0)) 4)
                                (9 (incf base (a)) 2)
                                (99 (return output))))
                  while input
                  finally (return (values output (list memory ip base))))))))))

(defun game (memory)
  (let (saves quicksave outputs commands)
    (labels ((state ()
               (list outputs commands memory))
             (load* (&optional state)
               (destructuring-bind (o c m) (or state (first saves))
                 (setf outputs o
                       commands c
                       memory m)
                 (format nil "~a~%~a" (first commands) (first outputs))))
             (save ()
               (push (state) saves)
               (format nil "Saved.~%"))
             (undo ()
               (load* quicksave))
             (toss ()
               (pop saves)
               (load*))
             (next ()
               (setf saves (append (rest saves) (list (first saves))))
               (load*)))
      (lambda (cmd)
        (case cmd
          (save (save))
          (load (load*))
          (undo (undo))
          (next (next))
          (toss (toss))
          (quit)
          (commands (reverse commands))
          (t
           (setf quicksave (state))
           (multiple-value-bind (output new-memory) (run memory cmd)
             (setf memory new-memory)
             (push cmd commands)
             (push output outputs)
             output)))))))

(defun input-stack (cmd)
  (when (plusp (length cmd))
    (setf cmd (format nil "~a~%" cmd)))
  (make-array (length cmd) :element-type 'character :fill-pointer t :initial-contents (reverse cmd)))

(defun cmd (string)
  (or (find string '("north" "south" "east" "west" "inv") :test #'str:starts-with?)
      (find string '(save load undo next toss quit) :test #'string-equal) string))

(defun play (game &optional replay)
  (loop for output = (funcall game "") then (funcall game (cmd (or (pop replay) (read-line))))
        and previous = output
        while output
        do (format t "~a" output)
        finally (return (values previous (funcall game 'commands)))))

(defun replay (memory moves)
  (loop while moves
        for (output new-memory) = (multiple-value-list (run memory (pop moves)))
        do (setf memory new-memory)
        finally (return (values output memory))))

(defparameter *replay* '("north"
                         "east"
                         "take astrolabe"
                         "south"
                         "take space law space brochure"
                         "north"
                         "north"
                         "north"
                         "take fuel cell"
                         "south"
                         "south"
                         "west"
                         "take easter egg"
                         "north"
                         "take manifold"
                         "north"
                         "north"
                         "take hologram"
                         "north"
                         "take weather machine"
                         "north"
                         "take antenna"
                         "west"
                         "inv"))

(defun inventory (output)
  (mapcar (a:rcurry #'subseq 2) (remove-if-not (a:curry #'str:starts-with? "- ") (aoc:lines output))))

(defun drops (inventory n)
  (loop for item in inventory
        for i from 0
        if (logtest n (ash 1 i))
          collect (format nil "drop ~a" item)))

(defun bruteforce (memory inventory)
  (loop for d from 0 to (expt 2 (length inventory))
        for output = (replay memory (append (drops inventory d) (list "south")))
        unless (str:contains? "== Security Checkpoint ==" output)
          return output))

(defun part1 (input)
  (multiple-value-bind (output memory) (replay (parse input) *replay*)
    (parse-integer (remove-if-not #'digit-char-p (bruteforce memory (inventory output))))))
