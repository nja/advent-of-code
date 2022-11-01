;;;; day17.lisp

(in-package #:aoc2018.day17)

(defun parse (line)
  (let ((coords (clean line)))
    (case (aref line 0)
      (#\x (mapcar (a:curry #'elt coords) '(0 0 1 2)))
      (#\y (mapcar (a:curry #'elt coords) '(1 2 0 0))))))

(defun clean (line)
  (mapcar #'parse-integer (remove-if (a:curry #'string= "") (ppcre:split "[^0-9]+" line))))

(defstruct (pos :conc-name (:constructor pos (x y))) x y)

(defun mark (scan slice)
  (destructuring-bind (x1 x2 y1 y2) scan
    (loop for x from x1 to x2 do
      (loop for y from y1 to y2 do
        (setf (gethash (pos x y) slice) #\#)))))

(defun slice (input)
  (let ((slice (make-hash-table :test 'equalp)))
    (mapc (a:compose (a:rcurry #'mark slice) #'parse) (aoc:lines input))
    slice))

(defun print-slice (slice water x y)
  (let ((y-max (second (limits slice))))
    (let ((col+ (- x 40))
          (row+ (max 0 (- y 25))))
      (aoc:print-indexed-lines
       (loop for row from row+ upto (max row+ (min y-max (+ y 25)))
             collect (with-output-to-string (s)
                       (loop for col from col+ upto (+ col+ 80)
                             for pos = (pos col row)
                             for c = (or (gethash pos slice)
                                         (gethash pos water)
                                         #\.)
                             do (princ c s))))
       :row+ row+ :col+ col+))))

(defun limits (slice)
  (loop for pos being the hash-keys in slice
        minimize (y pos) into y-min
        maximize (y pos) into y-max
        finally (return (list y-min y-max))))

(defun water ()
  (let ((water (make-hash-table :test 'equalp)))
    (setf (gethash (pos 500 0) water) #\+)
    water))

(defun flow (slice water wet max-y)
  (let (wettened)
    (labels ((wet? (p) (gethash p water))
             (flowing (p)
               (unless (wet? p)
                 (push p wettened))
               (setf (gethash p water) #\|))
             (still (p)
               (when (wet? (add p (pos 0 -1)))
                 (push (add p (pos 0 -1)) wettened))
               (remhash p water)
               (setf (gethash p slice) #\~)))
      (loop for w in wet
            for (left left-end) = (multiple-value-list (walk slice w -1))
            for (right right-end) = (multiple-value-list (walk slice w 1))
            for wetf = (if (and left-end right-end) #'still #'flowing)
            do (funcall wetf w)
               (mapc #'flowing (fall slice water w max-y))
               (mapc wetf left)
               (mapc wetf right))
      wettened)))

(defun walk (slice from dir)
  (flet ((occupied? (p) (gethash p slice)))
    (loop with d = (pos dir 0) and u = (pos 0 1)
          while (occupied? (add (or p from) u))
          for p = (add from d) then (add p d)
          until (occupied? p)
          collect p into result
          finally (return (values result (occupied? p))))))

(defun fall (slice water from max-y)
  (loop with d = (pos 0 1)
        for p = (add from d) then (add p d)
        while (<= (y p) max-y)
        until (or (gethash p slice) (gethash p water))
        collect p))

(defun add (p1 p2)
  (pos (+ (x p1) (x p2)) (+ (y p1) (y p2))))

(defun flood (slice)
  (loop with water = (water)
        with max-y = (second (limits slice))
        for w = (a:hash-table-keys water) then (flow slice water w max-y)
        while w
        finally (return water)))

(defun wet-count (slice water)
  (+ (still-count slice)
     (loop with (min-y max-y) = (limits slice)
           for pos being the hash-keys in water
           count (<= min-y (y pos) max-y))))

(defun part1 (input)
  (let ((slice (slice input)))
    (wet-count slice (flood slice))))

(defun still-count (slice)
  (loop for x being the hash-values in slice
        count (equal #\~ x)))

(defun part2 (input)
  (let ((slice (slice input)))
    (flood slice)
    (still-count slice)))
