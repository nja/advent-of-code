;;;; day20.lisp

(in-package :aoc2023.day20)

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

(defun modules (list)
  (let ((hash (make-hash-table)))
    (loop for module in list
          do (setf (gethash (id module) hash) module))
    (loop for src in list do
      (loop for dstid in (dests src)
            for dst = (gethash dstid hash)
            when (and dst (eq 'conjunction (mtype dst)))
              do (receive dst (id src) 'low #'list)))
    hash))

(defun receive (module source signal output)
  (flet ((send (sig)
           (dolist (dst (dests module))
             (funcall output (id module) sig dst))))
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

(defun process (modules wire buttons &optional cycles)
  (loop with highs = 0 and lows = 0
        for (src sig dst) = (queues:qpop wire)
        while sig
        do (case sig
             (high (incf highs))
             (low (incf lows)))
           (a:when-let (bs (and cycles (eq sig 'low) (gethash src cycles)))
             (when (/= (car bs) buttons)
               (push buttons (gethash src cycles))))
           (a:when-let (receiver (gethash dst modules))
             (receive receiver src sig
                      (lambda (&rest pulse) (queues:qpush wire pulse))))
        finally (return (list lows highs))))

(defun part1 (input)
  (let ((mods (modules (parse input)))
        (wire (queues:make-queue :simple-queue)))
    (loop for buttons from 1
          for (lows highs) = (progn (queues:qpush wire '(button low broadcaster))
                                    (process mods wire buttons))
          sum lows into totlows
          sum highs into tothighs
          repeat 1000
          finally (return (* totlows tothighs)))))

(defun part2 (input)
  (let ((mods (modules (parse input)))
        (wire (queues:make-queue :simple-queue))
        (cycles (make-hash-table)))
    (loop for mod in (a:hash-table-values mods)
          when (and (eq 'conjunction (mtype mod)) (< 1 (length (dests mod))))
            do (setf (gethash (id mod) cycles) (list 0)))
    (loop for buttons from 1
          do (queues:qpush wire '(button low broadcaster))
             (process mods wire buttons cycles)
          while (loop for l being the hash-values in cycles
                      thereis (< (length l) 2))
          finally (return (reduce #'lcm (mapcar (lambda (l) (- (first l) (second l)))
                                                (print (a:hash-table-values cycles))))))))
