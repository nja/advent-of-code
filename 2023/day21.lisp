;;;; day21.lisp

(in-package :aoc2023.day21)

(defparameter *test*
"...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........")

(defun start-pos (array)
  (loop for row below (array-dimension array 0) do
    (loop for col below (array-dimension array 1)
          when (char= #\S (aref array row col))
            do (return-from start-pos (list row col)))))

(defun parity (pos)
  (let ((x (reduce #'+ (mapcar (lambda (x)
                                 (if (zerop x)
                                     0
                                     (mod x 2)))
                               pos))))
    (if (zerop x)
        0
        (mod x 2))))

(defun neighbours (array &optional map-pos)
  (let ((rows (array-dimension array 0))
        (cols (array-dimension array 1)))
    (lambda (pos)
      (loop for d in '((-1 0) (1 0) (0 -1) (0 1))
            for p = (if map-pos
                        (funcall map-pos (add pos d) rows cols)
                        (add pos d))
            when (and (or map-pos (apply #'array-in-bounds-p array p))
                      (find (apply #'aref array p) ".S"))
              collect (add pos d)))))

(defun add (a b)
  (mapcar #'+ a b))

(defun parity-filter (pos &optional (test *parity-test*))
  (let ((parity (parity pos)))
    (lambda (p)
      (funcall test parity (parity p)))))

(defun count-steps (array steps start)
  (let ((nodes (remove-if-not (parity-filter start)
                              (dijkstra:search* start (neighbours array) :max-distance steps)
                              :key #'dijkstra:item)))
    (prog1 (length nodes)
      (when *trace*
        (let ((copy (a:copy-array array)))
          (dolist (pos (mapcar #'dijkstra:item nodes))
            (setf (apply #'aref copy pos) #\O))
          (terpri)
          (aoc:print-array copy))))))

(defun part1 (input)
  (let ((array (aoc:to-array input)))
    (count-steps array 64 (start-pos array))))

(defun mod-pos (pos rows cols)
  (list (mod (first pos) rows)
        (mod (second pos) cols)))

;; (defun wrapped-parity (pos rows cols)
;;   (let* ((p (mapcar (lambda (x)
;;                       (if (zerop x)
;;                           0
;;                           (mod x 2)))
;;                     pos))
;;          (wp (list (truncate (first pos) rows)
;;                    (truncate (second pos) cols))))
;;     (parity (add p wp))))

;; (defun wrapped-parity-filter (pos array)
;;   (let ((parity (parity pos))
;;         (rows (array-dimension array 0))
;;         (cols (array-dimension array 1)))
;;     (lambda (p)
;;       (equal parity (wrapped-parity p rows cols)))))

(defparameter *parity-test* #'=)

(defun count-steps* (array steps start)
  (count-if (parity-filter start)
            (dijkstra:search* start (neighbours array #'mod-pos) :max-distance steps)
            :key #'dijkstra:item))

;; (defun cycles (array steps)
;;   (let* ((start (start-pos array))
;;          (nodes (remove-if (parity-filter start)
;;                            (dijkstra:search* start (neighbours array #'mod-pos) :max-distance steps)
;;                            :key #'dijkstra:item)))
;;     (when (evenp steps) (decf steps))
;;     (loop for n from steps above 0 by 2
;;           for reached = (remove-if (lambda (d) (< n d))
;;                                    (or reached nodes)
;;                                    :key #'dijkstra:distance)
;;           collect (length reached))))

;; (defun diffs (cycles)
;;   (maplist (lambda (l) (- (first l) (if (cdr l) (second l) 0))) cycles))

(defun all-plots (array)
  (let* ((filter (parity-filter (start-pos array))))
    (loop for row below (array-dimension array 0)
          sum (loop for col below (array-dimension array 1)
                    count (and (funcall filter (list row col))
                               (find (aref array row col) "S."))))))

(defun centered-square-number (n)
  (if (< n 1)
      0
      (/ (1+ (expt (1- (* 2 n)) 2)) 2)))

(defparameter *small*
".....
.....
..S..
.....
.....")

(defparameter *trace* t)

(defun mongo (array steps)
  (let ((dim (midp array)))
    (multiple-value-bind (q rem) (truncate steps dim)
      (let* ((n q)
             (csn (centered-square-number n))
             (same-f (same-parity-size n))
             (other-f (other-parity-size n))
             (edges 4)
             (same-edges (if (and (plusp n) (evenp n))
                             edges
                             0))
             (other-edges (if (oddp n)
                              edges
                              0))
             (same-test (if (evenp steps)
                            #'=
                            #'/=))
             (other-test (if (oddp steps)
                             #'/=
                             #'=))
             (same-diagonals (if (evenp n)
                                 (max 0 (1- n))
                                 0))
             (other-diagonals (if (oddp n)
                                  (max 0 (1- n))
                                  0))
             (diag-n (1- rem)))
        (format t "q: ~a n: ~a diag-n: ~a~%" q n diag-n)
        (format t "csn: ~a~%" csn)
        (format t "same-f:  ~a other-f: ~a~%" same-f other-f)
        (format t "same-edges: ~a other-edges: ~a~%" same-edges other-edges)
        (format t "same-diagonals: ~a other-diagonals: ~a~%" same-diagonals other-diagonals)
        (+ (let ((*parity-test* same-test))
             (+ (count-steps array steps (start-pos array))
                (* same-f (all-plots array))
                (if (plusp same-edges)
                    (+ (top array diag-n)
                       (bottom array diag-n)
                       (left array diag-n)
                       (right array diag-n))
                    0)
                (if (plusp same-diagonals)
                    (* same-diagonals
                       (top-left array diag-n)
                       (top-right array diag-n)
                       (bottom-left array diag-n)
                       (bottom-right array diag-n))
                    0)))
           (let ((*parity-test* other-test))
             (+ (* other-f (all-plots array))
                (if (plusp other-edges)
                    (+ (top array diag-n)
                       (bottom array diag-n)
                       (left array diag-n)
                       (right array diag-n))
                    0)
                (if (plusp other-diagonals)
                    (* other-diagonals
                       (top-left array diag-n)
                       (top-right array diag-n)
                       (bottom-left array diag-n)
                       (bottom-right array diag-n))
                    0))))))))

;; 2023 21 2: '131453890353208129'
;; That's not the right answer.
;; (Cached until 2023-12-21 15:55:22)
;; 131453890353208129


(defun same-parity-size (n)
  (max 0 (1- (if (oddp n)
                 (* n n)
                 (* (1- n) (1- n))))))

(defun other-parity-size (n)
  (if (evenp n)
      (* n n)
      (* (1- n) (1- n))))

(defun odd-squares (n)
  (loop for i from 1 to n
        when (oddp i)
          sum (* i i)))

(defun part2 (input)
  (let ((answer (mongo (aoc:to-array input) 26501365)))
    (print (cond ((<= answer 600128497088216) 'too-low)
                 ((>= answer 603320653928855) (list 'too-high))
                 (t 'in-range)))
    answer))

;; (defun reach (array n)
;;   (let ((to-edge (midp array)))
;;     (if (< n to-edge)
;;         0
;;         (1+ (truncate (- n to-edge) (array-dimension array 0))))))

(defun top (array n)
  (+ (count-steps array n (list 0 (midp array)))
     (top-left array n)
     (top-right array n)))

(defun left (array n)
  (count-steps array n (list (midp array) 0)))

(defun top-left (array n)
  (count-steps array n (list 0 0)))

(defun top-right (array n)
  (count-steps array n (list 0 (maxp array))))

(defun bottom (array n)
  (count-steps array n (list (maxp array) (midp array))))

(defun right (array n)
  (+ (count-steps array n (list (midp array) (maxp array)))
     (top-right array n)
     (bottom-right array n)))

(defun bottom-right (array n)
  (count-steps array n (list (maxp array) (maxp array))))

(defun bottom-left (array n)
  (count-steps array n (list (maxp array) 0)))

(defun maxp (array)
  (1- (array-dimension array 0)))

(defun midp (array)
  (floor (array-dimension array 0) 2))

;; 2023 21 2: '84880831604'
;; That's not the right answer; your answer is too low.
;; Please wait one minute before trying again.
;; (Cached until 2023-12-21 10:06:47)
;; 84880831604

;;; 175005024074749729

;; 2023 21 2: '600128497088216'
;; That's not the right answer; your answer is too low.
;; Please wait one minute before trying again.
;; (Cached until 2023-12-21 11:55:51)
;; 600128497088216
;; (* 2 300076092415814)
;;   600152184831628
;; 2023 21 2: '600152184831628'
;; That's not the right answer.
;; (Cached until 2023-12-21 13:47:26)
;; 600152184831628

;; 49251015592451833736119210

;; 601727565726620

;; 600127018874809

;; 2023 21 2: '603320653928855'
;; That's not the right answer; your answer is too high.
;; Please wait one minute before trying again.
;; (Cached until 2023-12-21 12:09:21)
;; 603320653928855
;; 75019023168206

;; 2023 21 2: '601724637996755'
;; That's not the right answer.
;; (Cached until 2023-12-21 14:39:44)
;; 601724637996755



(defun mark-parity (array)
  (let ((filter (parity-filter (start-pos array))))
    (loop for row below (array-dimension array 0) do
      (loop for col below (array-dimension array 1)
            when (funcall filter (list row col))
              do (setf (aref array row col) #\O))))
  array)

(defun megaloop (array steps)
  (let* ((dim (array-dimension array 0))
         (size (1+ (truncate (truncate steps dim) 2)))
         (cutoff (* dim 2))
         (full-count (all-plots array))
         (cache (make-hash-table)))
    (flet ((get-size (f r)
             (let ((key (cons f r)))
               (or (gethash key cache)
                   (setf (gethash key cache)
                         (funcall f array r))))))
     (loop for bigrow from 0 to size
           sum (loop for bigcol from 0 to size
                     for bigdist = (+ (bigdist dim bigrow)
                                      (bigdist dim bigcol))
                     for rem = (- steps bigdist)
                     if (< cutoff rem)
                       sum (* 4 full-count)
                     else
                       sum (cond ((= 0 bigrow bigcol)
                                  (count-steps* array rem (start-pos array)))
                                 ((zerop bigrow)
                                 
                                  (+ (get-size #'left rem)
                                     (get-size #'right rem)))
                                 ((zerop bigcol)
                                 
                                  (+ (get-size #'top rem)
                                     (get-size #'bottom rem)))
                                 (t 
                                  (+ (get-size #'top-left rem)
                                     (get-size #'top-right rem)
                                     (get-size #'bottom-left rem)
                                     (get-size #'bottom-left rem))))
                     do (list bigrow bigcol rem))))))

(defun bigdist (dim b)
  (cond ((zerop b) 0)
        ((= 1 b) (ceiling dim 2))
        (t (+ (truncate dim 2)
              (* (1- b) dim)))))