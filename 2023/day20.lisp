;;;; day20.lisp

(in-package :aoc2023.day20)

(defparameter *test*
"broadcaster -> a, b, c
%a -> b
%b -> c
%c -> inv
&inv -> a")

(defun parse (input)
  (let ((*package* (symbol-package 'parse)))
    (mapcar (lambda (line)
              (destructuring-bind (left right) (str:split " -> " line)
                (flet ((name () (read-from-string left nil nil :start 1))
                       (destinations () (mapcar #'read-from-string (str:split ", " right))))
                  (case (aref left 0)
                    (#\b (make-module :id 'broadcaster :mtype 'broadcaster :dests (destinations)))
                    (#\% (make-module :id (name) :mtype 'flip-flop :dests (destinations)))
                    (#\& (make-module :id (name) :mtype 'conjunction :dests (destinations)
                                      :state (mapcan (lambda (d) (list d 'low))
                                                     (destinations))))))))
            (aoc:lines input))))

(defstruct (module :conc-name) id mtype state dests)

(defun receive (module source signal)
  (flet ((send (sig)
           (mapcar (lambda (dst) (list (id module) sig dst)) (dests module))))
    (case (mtype module)
      (broadcaster (send signal))
      (flip-flop (when (eq signal 'low)
                   (setf (state module) (not (state module)))
                   (send (if (state module) 'high 'low))))
      (conjunction
       (setf (getf (state module) source) signal)
       (send (if (find 'low (state module))
                 'high
                 'low))))))

