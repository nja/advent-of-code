;;;; day23.lisp

(in-package :aoc2022.day23)

(defun to-hash (input)
  (loop with hash = (make-hash-table :test 'equal)
        for y from 0
        for line in (aoc:lines input)
        do (loop for x from 0
                 for c across line
                 for pos = (pos x y)
                 when (eql #\# c)
                   do (setf (gethash pos hash) pos))
        finally (return hash)))

(defun to-array (input margin)
  (loop with rows = (+ (* 2 margin) (length (aoc:lines input)))
        with cols = (+ (* 2 margin) (length (first (aoc:lines input))))
        with array = (make-array (list rows cols) :initial-element #\.)
        for row from margin
        for line in (aoc:lines input)
        do (loop for col from margin
                 for c across line
                 do (setf (aref array row col) c))
        finally (return array)))

(defun grow (array)
  (loop with copy = (make-array (mapcar (a:curry #'+ 2) (array-dimensions array))
                                :initial-element #\.)
        for row from 0
        do (loop for col from 0
                 do (setf (aref copy (1+ row) (1+ col))
                          (aref array row col)))
           finally (return copy)))

(defmacro defcheck (name (&rest checks) &body result)
  `(defun ,name (nw n ne
                 w     e
                 sw s se)
     (declare (ignore ,@(set-difference '(nw n ne w e sw s se) checks)))
     (when (and ,@(mapcar (lambda (x) `(not (eql ,x #\#))) checks))
       ,@result)))

(defcheck check-north (nw n ne) '(0 -1))
(defcheck check-east (ne e se) '(1 0))
(defcheck check-south (sw s se) '(0 1))
(defcheck check-west (nw w sw) '(-1 0))
(defcheck check-all (nw n ne w e sw s se) t)

(defparameter *checks* nil)
(defun checks ()
   (a:circular-list #'check-north #'check-south #'check-west #'check-east))

(defun consider (pos nw n ne w e sw s se)
  (loop repeat 4
        for check in *checks*
        for d = (funcall check nw n ne w e sw s se)
        when d
          do (return (add pos d))))

(defun pos (x y)
  (list x y))

(defun add (a b)
  (mapcar #'+ a b))

(defun turn (array)
  (flet ((ref (row col)
           (when (array-in-bounds-p array row col)
             (aref array row col))))
    (let ((proposals (make-hash-table :test 'equal))
          (moved 0))
      (loop for row from 0 below (array-dimension array 0)
            do (loop for col from -1 below (array-dimension array 1)
                     for nw = n
                     for n = ne
                     for ne = (ref (1- row) (1+ col))
                     for w = x
                     for x = e
                     for e = (ref row (1+ col))
                     for sw = s
                     for s = se
                     for se = (ref (1+ row) (1+ col))
                     for pos = (pos col row)
                     when (and (eql #\# x) (not (check-all nw n ne w e sw s se)))
                       do (a:when-let (move (consider pos nw n ne w e sw s se))
                            (push pos (gethash move proposals)))))
      (maphash (lambda (destination elves)
                 (when (null (cdr elves))
                   (incf moved)
                   (destructuring-bind ((x1 y1)) elves
                     (destructuring-bind (x2 y2) destination
                       (rotatef (aref array y1 x1) (aref array y2 x2))))))
               proposals)
      (values array moved))))

(defun turns (array n)
  (dotimes (i n array)
    (turn array)
    (setf *checks* (cdr *checks*))))

(defun bounds (array)
  (loop with x1 and y1 and x2 and y2
        with n = 0
        for y below (array-dimension array 0)
        do (loop for x below (array-dimension array 1)
                 when (eql #\# (aref array y x))
                   do (setf x1 (if x1 (min x x1) x)
                            x2 (if x2 (max x x2) x)
                            y1 (if y1 (min y y1) y)
                            y2 (if y2 (max y y2) y))
                      (incf n))
        finally (return (list x1 y1 x2 y2 n))))

(defun count-empties (array)
  (destructuring-bind (x1 y1 x2 y2 n) (bounds array)
    (- (* (- x2 x1 -1) (- y2 y1 -1)) n)))

(defun part1 (input)
  (let ((*checks* (checks)))
    (count-empties (turns (to-array input 10) 10))))

(defun settle (array)
  (loop for i from 1
        when (zerop (nth-value 1 (turn array)))
          do (return i)
        do (setf *checks* (cdr *checks*))))

(defun part2 (input)
  (let ((*checks* (checks)))
    (settle (to-array input 100))))
