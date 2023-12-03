;;;; day07.lisp

(in-package :aoc2019.day07)

(defun run/c (memory)
  (let ((ip 0) output)
    (labels ((run ()
               (macrolet ((ref (i) `(aref memory ,i))
                          (rel (offset) `(aref memory (+ ip ,offset))))
                 (symbol-macrolet ((opcode (rel 0))
                                   (ia (rel 1))
                                   (ib (rel 2))
                                   (ic (rel 3)))
                   (symbol-macrolet ((pa (ref ia))
                                     (pb (ref ib))
                                     (pc (ref ic)))
                     (labels ((mode (i) (mod (truncate opcode (expt 10 (1+ i))) 10))
                              (a () (ecase (mode 1) (0 pa) (1 ia)))
                              (b () (ecase (mode 2) (0 pb) (1 ib)))
                              (jmp (x) (setf ip x) 0))
                       (loop with cont
                             do (incf ip (ecase (mod opcode 100)
                                           (1 (setf pc (+ (a) (b))) 4)
                                           (2 (setf pc (* (a) (b))) 4)
                                           (3 (setf cont (let ((address ia))
                                                           (lambda (x)
                                                             (setf (ref address) x)
                                                             (run))))
                                            2)
                                           (4 (push (a) output) 2)
                                           (5 (if (not (zerop (a))) (jmp (b)) 3))
                                           (6 (if (zerop (a)) (jmp (b)) 3))
                                           (7 (setf pc (if (< (a) (b)) 1 0)) 4)
                                           (8 (setf pc (if (= (a) (b)) 1 0)) 4)
                                           (99 (return (values nil output)))))
                             when cont return (values cont output))))))))
      (run))))

(defun amplifiers (memory settings)
  (mapcar (lambda (setting)
            (funcall (run/c (a:copy-array memory)) setting))
          settings))

(defun amplify (amplifiers &optional feedback)
  (loop with signal = 0
        for conts = (mapcar (lambda (amp)
                              (multiple-value-bind (cont output)
                                  (funcall amp signal)
                                (setf signal (car output))
                                cont))
                            (or conts amplifiers))
        while (and feedback (not (member nil conts)))
        finally (return signal)))

(defun max-permutations (f list)
  (let ((max 0))
    (a:map-permutations (lambda (p)
                          (setf max (max max (funcall f p))))
                        list)
    max))

(defun max-signal (memory settings &key feedback)
  (max-permutations (lambda (settings)
                      (amplify (amplifiers memory settings) feedback))
                    settings))

(defun part1 (input)
  (max-signal (parse input) '(0 1 2 3 4)))

(defun part2 (input)
  (max-signal (parse input) '(5 6 7 8 9) :feedback t))