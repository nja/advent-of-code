;;;; day18.lisp

(in-package :aoc2023.day18)

(defparameter *test*
"R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)")

(defun parse (input)
  (mapcar (lambda (l)
            (destructuring-bind (a b c) (str:split " " l)
              (list (intern a (symbol-package 'parse))
                    (parse-integer b)
                    (parse-integer c :start 2 :end 8 :radix 16))))
          (aoc:lines input)))

(defun dig (plan)
  (let ((hash (make-hash-table :test 'equal))
        (pos '(0 0)))
    (dolist (op plan hash)
      (setf pos (apply #'move hash pos op)))))

(defun move (hash pos dir steps color)
  (let ((d (delta dir)))
    (dotimes (i steps pos)
      (setf pos (add pos d))
      (setf (gethash pos hash) color))))

(defun delta (dir)
  (case dir
    (U '(-1  0))
    (D '( 1  0))
    (L '( 0 -1))
    (R '( 0  1))))

(defun add (a b)
  (mapcar #'+ a b))

(defun boundaries (hash)
  (loop for (a b) in (a:hash-table-keys hash)
        minimize a into min-x
        minimize b into min-y
        maximize a into max-x
        maximize b into max-y
        finally (return (list (1- min-x) (1- min-y) (1+ max-x) (1+ max-y)))))

(defun count-interior (hash)
  (destructuring-bind (min-x min-y max-x max-y) (boundaries hash)
    (flet ((neighbours (pos)
             (loop for dir in '(U D L R)
                   for n = (add pos (delta dir))
                   for (x y) = n
                   when (and (<= min-x x max-x)
                             (<= min-y y max-y)
                             (not (gethash n hash)))
                     collect n)))
      (- (* (1+ (- max-x min-x))
            (1+ (- max-y min-y)))
         (length (dijkstra:search* (list min-x min-y) #'neighbours))))))

(defun decode (plan)
  (loop for x in (mapcar #'third plan)
        for dir = (nth (logand x #xf) '(R D L U))
        for len = (ash x -4)
        collect (list dir len)))

(defun positions (plan)
  (loop for (dir len) in plan
        for prev = '(0 0) then pos
        for pos = (add prev (mul (delta dir) len))
        collect pos))

(defun mul (a f)
  (mapcar (a:curry #'* f) a))

(defun verticals-hash (positions)
  (loop with hash = (make-hash-table)
        for (pr pc) = '(0 0) then (list r c)
        for (r c) in positions
        when (eql c pc)
          do (push (list (min r pr) (max r pr)) (gethash c hash))
        finally (return hash)))

(defun corners (plan)
  (loop for prev in (mapcar #'car plan)
        for dir in (a:rotate (mapcar #'car plan) -1)
        when prev 
          collect (case prev
                    (U (ecase dir (L 7) (R 'F)))
                    (D (ecase dir (L 'J) (R 'L)))
                    (L (ecase dir (U 'L) (D 'F)))
                    (R (ecase dir (U 'J) (D 7))))))

(defun corners-hash (plan)
  (loop with hash = (make-hash-table :test 'equal)
        for last-pos = '(0 0) then (list r c)
        for (pr pc) = last-pos
        for (r c) = pos
        when (eql c pc)
          do (setf (gethash last-pos hash) (if (< pr r)
                                               '))
        finally (return hash)))

(lambda (x y)
  (a:when-let (l (gethash y hash))
    (loop for (min-x max-x) in l
            thereis (< min-x x max-x))))

(defun part2 (input)
  (decode (parse input)))