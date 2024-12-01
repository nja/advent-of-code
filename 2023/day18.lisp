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

(defun part1 (input)
  (count-interior (dig (parse input))))

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

(defun verticals-predicate (plan)
  (let ((hash (verticals-hash (positions plan))))
    (lambda (r c)
      (a:when-let (l (gethash c hash))
        (and (loop for (min-r max-r) in l
                     thereis (< min-r r max-r))
             'vert)))))

(defun corners (plan)
  (loop for prev in (mapcar #'car plan)
        for dir in (a:rotate (mapcar #'car plan) -1)
        when prev
          collect (case prev
                    (U (ecase dir (L 7) (R 'F)))
                    (D (ecase dir (L 'J) (R 'L)))
                    (L (ecase dir (U 'L) (D 'F)))
                    (R (ecase dir (U 'J) (D 7))))))

(defun left-corners-hash (plan)
  (loop with hash = (make-hash-table :test 'equal)
        with positions = (positions plan)
        with corners = (corners plan)
        for prev-pos in (a:rotate positions)
        for prev-corner in (a:rotate corners)
        for pos in positions
        for corner in corners
        for (pr pc) = prev-pos
        for (r c) = pos
        when (eql r pr)
          do (let ((left-col (min pc c)))
               (setf (gethash (list r left-col) hash) (if (< pc c)
                                                          prev-corner
                                                          corner)))
        finally (return hash)))


(defun right-corners-hash (plan)
  (loop with hash = (make-hash-table :test 'equal)
        with positions = (positions plan)
        with corners = (corners plan)
        for prev-pos in (a:rotate positions)
        for prev-corner in (a:rotate corners)
        for pos in positions
        for corner in corners
        for (pr pc) = prev-pos
        for (r c) = pos
        when (eql r pr)
          do (let ((right-col (max pc c)))
               (setf (gethash (list r right-col) hash) (if (< pc c)
                                                           corner
                                                           prev-corner)))
        finally (return hash)))

(defun state (state pipe)
  (if (not pipe)
      state
      (case state
        (out (ecase pipe
               (vert 'in)
               (F 'out-F)
               (L 'out-L)))
        (in (ecase pipe
              (vert 'out)
              (F 'in-F)
              (L 'in-L)))
        (out-F (ecase pipe
                 (J 'in)
                 (7 'out)))
        (out-L (ecase pipe
                 (J 'out)
                 (7 'in)))
        (in-F (ecase pipe
                (J 'out)
                (7 'in)))
        (in-L (ecase pipe
                (J 'in)
                (7 'out))))))

(defun pos-pred (hash)
  (lambda (r c) (gethash (list r c) hash)))

(defun interesting-columns (plan)
  (remove-duplicates (sort (mapcar #'second (positions plan))
                           #'<)))

(defun rows (plan)
  (let ((rows (mapcar #'first (positions plan))))
    (list (reduce #'min rows) (reduce #'max rows))))

(defun interesting-rows (plan)
  (sort (remove-duplicates (mapcar #'first (positions plan))) #'<))

(defun interesting-row-predicate (plan)
  (let ((hash (make-hash-table)))
    (dolist (row (interesting-rows plan))
      (setf (gethash row hash) t))
    (lambda (row) (or (gethash row hash)
                      (gethash (1- row) hash)))))

(defun row-insides (row cols vert? left? right?)
  (loop with inside-from
        with sum = 0
        for state = 'out then new-state
        for col in cols
        for pipe = (or (funcall vert? row col)
                       (funcall left? row col)
                       (funcall right? row col))
        for new-state = (state state pipe)
        if (not (eq state new-state))
          do ;(format t "~& row ~d col ~d ~a -> ~a" row col state new-state)
             (cond ((eq new-state 'in) (setf inside-from col))
                   ((eq state 'in)
                    (incf sum (- col inside-from 1))
                    (setf inside-from nil)))
        finally (return sum)))

(defun border (plan)
  (loop for prev in (a:rotate (positions plan))
        for pos in (positions plan)
        sum (dist prev pos)))

(defun dist (a b)
  (reduce #'+ (mapcar (lambda (x y) (abs (- x y))) a b)))

(defun lagoon-size (plan)
  (+ (border plan)
     (loop with vert? = (verticals-predicate plan)
           with left? = (pos-pred (left-corners-hash plan))
           with right? = (pos-pred (right-corners-hash plan))
           with interesting-row? = (interesting-row-predicate plan)
           with (min-row max-row) = (rows plan)
           with cols = (interesting-columns plan)
           for row from min-row to max-row
           for rowsize = (if (funcall interesting-row? row)
                             (row-insides row cols vert? left? right?)
                             rowsize)
           sum rowsize)))

(defun part2 (input)
  (lagoon-size (decode (parse input))))


 ;;  0123456
 ;; 0L######
 ;; 1#.....#
 ;; 2L#R...#
 ;; 3..#...#
 ;; 4..#...#
 ;; 5L#R.L#R
 ;; 6#...#..
 ;; 7LR..L#R
 ;; 8.#....#
 ;; 9.L#####



 ;;  0123456
 ;; 0#######
 ;; 1#.....#
 ;; 2###...#
 ;; 3..#...#
 ;; 4..#...#
 ;; 5###.###
 ;; 6#...#..
 ;; 7##..###
 ;; 8.#....#
 ;; 9.######

(defparameter *test2*
"R 2 (#70c710)
U 2 (#70c710)
R 2 (#70c710)
D 2 (#70c710)
R 2 (#70c710)
U 2 (#70c710)
R 2 (#70c710)
D 6 (#70c710)
L 2 (#70c710)
U 2 (#70c710)
L 2 (#70c710)
D 2 (#70c710)
L 2 (#70c710)
U 2 (#70c710)
L 2 (#70c710)
U 2 (#70c710)")

;;  012345678
;; 2  L#R L#R
;; 1  # # # #
;; 0L#R L## #
;; 1#       #
;; 2L#R L#R #
;; 3  # # # #
;; 4  L#R L#R

(defun mark-corners (array hash rd)
  (loop for (r c) in (a:hash-table-keys hash)
        do (setf (aref array (+ r rd) c) (gethash (list r c) hash))))


