;;;; day10.lisp

(in-package :aoc2023.day10)

(defun turn (pipe dir)
  (ecase pipe
    (#\| (ecase dir (S 'S) (N 'N)))
    (#\- (ecase dir (W 'W) (E 'E)))
    (#\L (ecase dir (W 'N) (S 'E)))
    (#\J (ecase dir (S 'W) (E 'N)))
    (#\7 (ecase dir (E 'S) (N 'W)))
    (#\F (ecase dir (W 'S) (N 'E)))
    (#\S)))

(defun dir (direction)
  (getf '(N (-1  0) S ( 1  0) E ( 0  1) W ( 0 -1)) direction))

(defun add (a b)
  (mapcar #'+ a b))

(defun start-position (array)
  (loop for row below (array-dimension array 0) do
    (loop for col below (array-dimension array 1)
          when (eql #\S (aref array row col))
            do (return-from start-position (list row col)))))

(defun pipe (array position direction)
  (loop for p = (add position (dir direction)) then (add p (dir dir))
        for pipe = (apply #'aref array p)
        for dir = (turn pipe (or dir direction))
        collect p
        until (null dir)))

(defun part1 (input)
  (let ((array (aoc:to-array input)))
    (/ (length (pipe array (start-position array) 'S)) 2)))

(defun count-insides (array pipe?)
  (loop for row below (array-dimension array 0)
        sum (loop for col below (array-dimension array 1)
                  for p = (and (funcall pipe? row col)
                               (aref array row col))
                  for state = (state 'out p) then (state state p)
                  count (and (not p) (eq state 'in)))))

(defun state (state pipe)
  (if (not pipe)
      state
      (case state
        (out (ecase pipe
               ((#\| #\S) 'in)
               (#\F 'out-F)
               (#\L 'out-L)))
        (in (ecase pipe
              ((#\| #\S) 'out)
              (#\F 'in-F)
              (#\L 'in-L)))
        (out-F (ecase pipe
                 (#\- 'out-F)
                 (#\J 'in)
                 (#\7 'out)))
        (out-L (ecase pipe
                 (#\- 'out-L)
                 (#\J 'out)
                 (#\7 'in)))
        (in-F (ecase pipe
                (#\- 'in-F)
                (#\J 'out)
                (#\7 'in)))
        (in-L (ecase pipe
                (#\- 'in-L)
                (#\J 'in)
                (#\7 'out))))))

(defun pipe-predicate (pipe)
  (let ((set (make-hash-table :test 'equal)))
    (dolist (p pipe)
      (setf (gethash p set) t))
    (lambda (&rest key)
      (gethash key set))))

(defun part2 (input)
  (let ((array (aoc:to-array input)))
    (count-insides array (pipe-predicate (pipe array (start-position array) 'S)))))
