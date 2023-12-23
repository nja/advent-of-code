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
  (aoc:to-array input))

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

(defun letters (from to)
  (with-output-to-string (s)
    (loop for c from (char-code from) to (char-code to)
          do (princ (code-char c) s))))

(defparameter *letters* (str:concat (letters #\A #\Z) (letters #\a #\z)))

(defun letter (i) (aref *letters* (mod i (length *letters*))))

(defun part1 (input)
  (walk (parse input) '(0 1) 0 0))

;; (defun dfs (array ))

(defun max-walkable (array)
  (loop for i below (array-total-size array)
        count (eql (row-major-aref array i) #\.)))

(defparameter *max-distances* nil)

(defun klaw (array pos steps)
  (if (and nil (<= steps (gethash pos *max-distances* -1)))
      0
      (let ((options (neighbours array pos)))
        (setf (apply #'aref array pos) steps)
        (ecase (length options)
          (0 (if (= (1- (array-dimension array 0)) (first pos))
                 (prog1 steps
                   ;; (record-distances array)
                   ;; (terpri)
                   ;; (aoc:print-array array)
                   ;; (format t "~&Steps: ~a~%" steps)
                   ;; (break)
                         
                   )
                 0))
          (1 (klaw array (first options) (1+ steps)))
          ((2 3) (loop for n in options
                       maximize (klaw (a:copy-array array) n (1+ steps))))))))

(defun record-distances (array)
  (loop for row below (array-dimension array 0) do
    (loop for col below (array-dimension array 1)
          for pos = (list row col)
          for x = (aref array row col)
          when (integerp x)
            do (setf (gethash pos *max-distances*)
                     (max x (gethash pos *max-distances* -1))))))


(defun neighbours* (array pos steps)
  (let ((n '(-1 0)) (s '(1 0)) (e '(0 1)) (w '(0 -1))) 
    (loop for d in (list e s w n)
          for p = (add pos d)
          for x = (and (apply #'array-in-bounds-p array p)
                       (apply #'aref array p))
          when (or (eql #\. x)
                   (and (integerp x) (< x steps)))
            collect p)))

(defun parse* (input)
  (aoc:to-array (aoc:tr "<>v^" "...." input)))

(defun part2 (input)
  (let* ((*max-distances* (make-hash-table :test 'equal)))
    (klaw (parse* array) '(0 1) 0)))

(defun analyze (array pos branch)
  (setf (apply #'aref array pos) (letter branch))
  (let ((options (neighbours array pos)))
    (if (= 1 (length options))
        (analyze array (first options) branch)
        (loop for b from (1+ branch)
              for n in options
              do (analyze array n b)))))

;;; I branch A kommer jag inte heller kunna välja B
;;; I branch B kommer jag inte heller kunna välja A!!!

(defun max-neighbours (array)
  (loop for row below (array-dimension array 0)
        maximize (loop for col below (array-dimension array 1)
                       for c = (aref array row col)
                       maximize (if (eql c #\.)
                                    (length (neighbours array (list row col)))
                                    0))))

(defun no-double-branches (array)
  (loop for row below (array-dimension array 0)
        always (loop for col below (array-dimension array 1)
                     for c = (aref array row col)
                     always (or (not (eql c #\.))
                                (< (length (neighbours array (list row col))) 3)
                                (loop for n in (neighbours array (list row col))
                                      always (eql 2 (length (neighbours array n))))))))
