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

(defparameter *parity-test* #'=)

(defun parity-filter (pos &optional (test *parity-test*))
  (let ((parity (parity pos)))
    (lambda (p)
      (funcall test parity (parity p)))))

(defun count-steps (array steps start)
  (if (plusp steps)
      (let ((nodes (remove-if-not (parity-filter start)
                                  (dijkstra:search* start (neighbours array) :max-distance steps)
                                  :key #'dijkstra:item)))
        (prog1 (length nodes)
          (when *trace*
            (let ((copy (a:copy-array array)))
              (dolist (pos (mapcar #'dijkstra:item nodes))
                (setf (apply #'aref copy pos) #\O))
              (terpri)
              (aoc:print-array copy)))))
      0))

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

(defparameter *small* (aoc:to-array
".....
.....
..S..
.....
....."))


(defparameter *medium* (aoc:to-array
".......
.......
.......
...S...
.......
.......
......."))


(defparameter *trace* t)

(defun mongo (array steps)
  (let* ((*parity-test* (if (evenp steps)
                            #'=
                            #'/=))
         (dim (array-dimension array 0))
         (steps-to-edge (midp array))
         (steps-for-skipping (max 0 (- steps steps-to-edge dim)))
         (steps-in-last (- steps steps-to-edge steps-for-skipping)))
    (multiple-value-bind (skipped rem) (truncate steps-for-skipping dim)
      (when (plusp skipped) (assert (eql rem (mod steps-in-last dim))))
      (let* ((n (1+ skipped))
             (csn (centered-square-number n))
             (same-diagonals (same-diagonals n))
             (other-diagonals (other-diagonals n))
             (en steps-in-last)
             (dn (1- rem)))
        (format t "dim: ~a skipped: ~a~%" dim skipped)
        (format t "steps-to-edge: ~a steps-for-skipping: ~a~%" steps-to-edge steps-for-skipping)
        (format t "steps-in-last: ~a~%" steps-in-last)
        (format t "rem: ~a n: ~a csn: ~a~%" rem n csn)
        ;; (format t "en: ~a dn: ~a~%" en dn)
        (+ 
         (+ (count-steps array steps (start-pos array))
            (* csn (all-plots array))
            (if (plusp (same-edges n))
                (+ (top array en)
                   (bottom array en)
                   (left array en)
                   (right array en))
                0))
         (if (plusp same-diagonals)
             (* same-diagonals
                (top-left array dn)
                (top-right array dn)
                (bottom-left array dn)
                (bottom-right array dn))
             0)
         (let ((*parity-test* (if (evenp steps)
                                  #'/=
                                  #'=)))
           (+ (if (plusp (other-edges n))
                  (+ (top array en)
                     (bottom array en)
                     (left array en)
                     (right array en))
                  0)
              (if (plusp other-diagonals)
                  0 0)))
         )))))

;; 2023 21 2: '131453890353208129'
;; That's not the right answer.
;; (Cached until 2023-12-21 15:55:22)
;; 131453890353208129


(defun other-edges (n)
  (cond ((minusp n) (error "neg"))
        ((zerop n) 0)
        ((oddp n) 4)
        ((evenp n) 0)))

(defun same-edges (n)
  (cond ((minusp n) (error "neg"))
        ((zerop n) 0)
        ((oddp n) 0)
        ((evenp n) 4)))

(defun same-diagonals (n)
  (cond ((minusp n) (error "neg"))
        ((zerop n) 0)
        ((evenp n) ())))

(defun other-diagonals (n)
  (cond ((minusp n) (error "neg"))
        ((zerop n) 0)
        (t 0)))


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
  (when *trace* (print (list 'top n)))
  (count-steps array n (list 0 (midp array))))

(defun left (array n)
  (when *trace* (print (list 'left n)))
  (count-steps array n (list (midp array) 0)))

(defun top-left (array n)
  (when *trace* (print (list 'top-left n)))
  (count-steps array n (list 0 0)))

(defun top-right (array n)
  (when *trace* (print (list 'top-right n)))
  (count-steps array n (list 0 (maxp array))))

(defun bottom (array n)
  (when *trace* (print (list 'bottom n)))
  (count-steps array n (list (maxp array) (midp array))))

(defun right (array n)
  (when *trace* (print (list 'right n)))
  (count-steps array n (list (midp array) (maxp array))))

(defun bottom-right (array n)
  (when *trace* (print (list 'bottom-right n)))
  (count-steps array n (list (maxp array) (maxp array))))

(defun bottom-left (array n)
  (when *trace* (print (list 'bottom-left n)))
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