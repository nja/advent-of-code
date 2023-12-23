;;;; day23.lisp

(in-package :aoc2023.day23)

(defparameter *test*
"#.#####################
#.......#########...###
#######.#########.#.###
###.....#.>.>.###.#.###
###v#####.#v#.###.#.###
###.>...#.#.#.....#...#
###v###.#.#.#########.#
###...#.#.#.......#...#
#####.#.#.#######.#.###
#.....#.#.#.......#...#
#.#####.#.#.#########v#
#.#...#...#...###...>.#
#.#.#v#######v###.###v#
#...#.>.#...>.>.#.###.#
#####v#.#.###v#.#.###.#
#.....#...#...#.#.#...#
#.#########.###.#.#.###
#...###...#...#...#.###
###.###.#.###v#####v###
#...#...#.#.>.>.#.>.###
#.###.###.#.###.#.#v###
#.....###...###...#...#
#####################.#")

(defun parse (input)
  (let ((array (aoc:to-array input)))
    (setf (aref array 0 1) #\S)
    array))

(defun neighbours (array pos)
  (let ((n '(-1 0)) (s '(1 0)) (e '(0 1)) (w '(0 -1))) 
    (loop for d in (list e s w n)
          for p = (add pos d)
          for c = (and (apply #'array-in-bounds-p array p)
                       (apply #'aref array p))
          when (case c
                 (#\. t)
                 (#\< (eq d w))
                 (#\> (eq d e))
                 (#\v (eq d s))
                 (#\^ (eq d n)))
            collect p)))

(defun add (a b)
  (mapcar #'+ a b))

(defun walk (array pos steps branch)
  (let ((options (neighbours array pos)))
    (setf (apply #'aref array pos) (letter branch))
    (ecase (length options)
      (0 (if (= (1- (array-dimension array 0)) (first pos))
             steps
             0))
      (1 (walk array (first options) (1+ steps) branch))
      ((2 3) (reduce #'max (mapcar (lambda (n)
                                     (walk (a:copy-array array)
                                           n
                                           (1+ steps)
                                           (prog1 branch (incf branch))))
                                   options))))))

(defparameter *letters* (str:concat (letters #\A #\Z) (letters #\a #\z)))

(defun letter (i) (aref *letters* (mod i (length *letters*))))

(defun letters (from to)
  (with-output-to-string (s)
    (loop for c from (char-code from) to (char-code to)
          do (princ (code-char c) s))))

(defun part1 (input)
  (walk (parse input) '(0 1) 0 0))