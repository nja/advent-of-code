;;;; day20.lisp

(in-package :aoc2023.day20)

(defparameter *test*
"broadcaster -> a, b, c
%a -> b
%b -> c
%c -> inv
&inv -> a")

(defparameter *test2*
"broadcaster -> a
%a -> inv, con
&inv -> b
%b -> con
&con -> output")

(defun parse (input)
  (let ((*package* (symbol-package 'parse)))
    (mapcar (lambda (line)
              (destructuring-bind (left right) (str:split " -> " line)
                (flet ((name () (read-from-string left nil nil :start 1))
                       (destinations () (mapcar #'read-from-string (str:split ", " right))))
                  (case (aref left 0)
                    (#\b (make-module :id 'broadcaster :mtype 'broadcaster :dests (destinations)))
                    (#\% (make-module :id (name) :mtype 'flip-flop :dests (destinations)))
                    (#\& (make-module :id (name) :mtype 'conjunction :dests (destinations)))))))
            (aoc:lines input))))

(defstruct (module :conc-name) id mtype state dests)

(defun modules (modules)
  (let ((hash (make-hash-table)))
    (loop for module in modules
          do (setf (gethash (id module) hash) module))
    (loop for src in modules do
      (loop for dstid in (dests src)
            for dst = (gethash dstid hash)
            when (and dst (eq 'conjunction (mtype dst)))
              do (receive dst (id src) 'low #'list)))
    hash))

(defun receive (module source signal output)
  (when module
    (flet ((send (sig)
             (map nil (lambda (dst) (funcall output (id module) sig dst)) (dests module))))
      (case (mtype module)
        (broadcaster (send signal))
        (flip-flop (when (eq signal 'low)
                     (setf (state module) (not (state module)))
                     (send (if (state module) 'high 'low))))
        (conjunction
                                        ;(format t "~a state was ~a" (id module) (state module))
         (setf (getf (state module) source) signal)
                                        ;(format t " is ~a~%" (state module))
         (send (if (every (lambda (x)
                            (case x
                              (high t)
                              (low nil)
                              (t t)))
                          (state module))
                   'low
                   'high)))))))

(defun process (modules wire)
  (loop with highs = 0 and lows = 0
        for (src sig dst) = (queues:qpop wire)
        while sig
        do ;(format t "~a -~a-> ~a~%" src sig dst)
           (case sig
             (high (incf highs))
             (low (incf lows)))
           (receive (gethash dst modules) src sig
                    (lambda (&rest pulse) (queues:qpush wire pulse)))
        finally (return (list lows highs))))

(defun wire ()
  (queues:make-queue :simple-queue))

(defun part1 (input)
  (let ((mods (modules (parse input)))
        (wire (wire)))
    (loop repeat 1001
          for (lows highs) = (process mods wire)
          do (queues:qpush wire (list 'button 'low 'broadcaster))
          sum lows into totlows
          sum highs into tothighs
          finally (return (* totlows tothighs)))))