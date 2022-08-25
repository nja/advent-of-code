;;;; day25.lisp

(in-package #:aoc2021.day25)

(defun parse-input (input)
  (flet ((line-parser (from to)
           (lambda (line)
             (parse-integer (aoc:tr from to line) :radix 2))))
    (let ((lines (aoc:lines input)))
      (list (apply #'vector (mapcar (line-parser ".v>" "001") lines))
            (apply #'vector (mapcar (line-parser ".>v" "001") lines))
            (apply #'vector lines)
            (length (first lines))))))

(defun settle (state)
  (loop for i from 1
        until (zerop (nmove state))
        finally (return i)))

(defun nmove (state)
  (destructuring-bind (right-movers down-movers tmp width) state
    (let ((count 0))
      (incf count (move-right right-movers down-movers tmp (ash 1 (1- width)) (1- (ash 1 width))))
      (rotatef right-movers tmp)
      (incf count (move-down right-movers down-movers tmp))
      (rotatef down-movers tmp)
      (setf (first state) right-movers
            (second state) down-movers
            (third state) tmp)
      count)))

(defun move-right (right-movers others target msb mask)
  (flet ((shift-left (x) (logand mask (+ (ash x 1) (if (plusp (logand msb x)) 1 0))))
         (shift-right (x) (+ (ash x -1) (if (oddp x) msb 0))))
    (loop for i from 0
          for o across others
          for rm across right-movers
          for blocked = (logior rm o)
          for moving = (logandc2 (shift-right rm) blocked)
          for staying = (logand rm (shift-left blocked))
          summing (logcount moving)
          do (setf (svref target i) (logior staying moving)))))

(defun move-down (others down-movers target)
  (labels ((wrap (i) (mod i (length target)))
           (leaving (from-i)
             (logandc2 (svref down-movers (wrap from-i))
                       (logior (svref down-movers (wrap (1+ from-i)))
                               (svref others (wrap (1+ from-i)))))))
    (loop for i from 0 below (length target)
          for incoming = (leaving (1- i)) then leaving
          for leaving = (leaving i)
          summing (logcount leaving)
          when (< i (length target))
            do (setf (svref target i)
                     (logior (logandc2 (svref down-movers i) leaving)
                             incoming)))))

(defun part1 (input)
  (settle (parse-input input)))
