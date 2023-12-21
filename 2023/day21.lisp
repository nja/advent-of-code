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


(defparameter *ofs* '(0 0))

;; (defun count-steps (array steps start)
;;   (if (plusp steps)
;;       (let ((nodes (remove-if-not (parity-filter (list (midp array) (midp array)))
;;                                   (dijkstra:search* start (neighbours array) :max-distance steps)
;;                                   :key #'dijkstra:item)))
;;         (prog1 (length nodes)
;;           (when (and t *trace*)
;;             (dolist (pos (mapcar #'dijkstra:item nodes))
;;               (setf (apply #'aref *trace* (add *ofs* pos)) (cond ((eql *parity-test* #'=) #\O)
;;                                                                  ((eql *parity-test* #'/=) #\X)
;;                                                                  (t #\?)))))))
;;       0))


(defun count-steps (array steps start)
  (if (plusp steps)
      (let ((nodes (remove-if-not (parity-filter start)
                                  (dijkstra:search* start (neighbours array) :max-distance steps)
                                  :key #'dijkstra:item)))
        (prog1 (length nodes)
          (when (and t *trace*)
            (let ((copy (a:copy-array array)))
              (dolist (pos (mapcar #'dijkstra:item nodes))
                (setf (apply #'aref copy pos) (cond ((eql *parity-test* #'=) #\O)
                                                    ((eql *parity-test* #'/=) #\X)
                                                    (t #\?))))
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

;; (defun all-plots (array)
;;   (let* ((filter (parity-filter (start-pos array))))
;;     (loop for row below (array-dimension array 0)
;;           sum (loop for col below (array-dimension array 1)
;;                     count (and (funcall filter (list row col))
;;                                (find (aref array row col) "S."))))))
(defun all-plots (array)
  (count-steps array (array-total-size array) (start-pos array)))

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
         (steps-past-edge (- steps steps-to-edge))
         (skipped (max 0 (1- (truncate steps-past-edge dim))))
         (skipped-steps (* skipped dim))
         (steps-in-last (- steps steps-to-edge skipped-steps)))
    (assert (eql steps (+ steps-to-edge skipped-steps steps-in-last)))
    (let* ((n (truncate steps dim))
           (mid 1)
           (csn (centered-square-number n))
           (same-edges (same-edges n))
           (other-edges (other-edges n))
           (same-diagonals (same-diagonals n))
           (other-diagonals (other-diagonals n))
           (same-skipped (same-skipped n))
           (other-skipped (other-skipped n))
           (en steps-in-last)
           (1qn (- steps-in-last steps-to-edge 1))
           (3qn (+ en 1qn))
           (full-tot (+ mid same-skipped other-skipped)))
      (format t "dim: ~a~%" dim)
      (format t "skipped: ~a~%" skipped)
      (format t "steps-to-edge: ~a~%" steps-to-edge)
      (format t "steps-past-edge: ~a~%" steps-past-edge)
      (format t "skipped-steps: ~a~%" skipped-steps)
      (format t "steps-in-last: ~a~%" steps-in-last)
      (format t "en: ~a n: ~a csn: ~a~%" en n csn)
      (format t "same-diagonals: ~a other-diagonals: ~a~%" same-diagonals other-diagonals)
      (format t "same-skipped: ~a other-skipped: ~a~%" same-skipped other-skipped)
      (format t "same-edges: ~a other-edges: ~a~%" same-edges other-edges)
      (format t "en: ~a 1qn: ~a 3qn: ~a~%" en 1qn 3qn)
      (format t "full-tot: ~a csn: ~a~%" full-tot (centered-square-number n))
      (assert (evenp n))
      (when (plusp n) (assert (equal full-tot (centered-square-number n))))
      (+ 
       (+ (count-steps array steps (start-pos array))
          (if (plusp same-skipped)
              (* same-skipped (all-plots array))
              0)
          (if (plusp same-edges)
              (+ (top array en)
                 (bottom array en)
                 (left array en)
                 (right array en))
              0)
          (if (plusp same-diagonals)
              (+ (top-left array 3qn)
                 (top-right array 3qn)
                 (bottom-left array 3qn)
                 (bottom-right array 3qn))
              0))
       (let ((*parity-test* (if (evenp steps)
                                #'/=
                                #'=)))
         (+ (if (plusp other-skipped)
                (* other-skipped (all-plots array))
                0)
            ;; (if (plusp other-edges)
            ;;     (+ (top array en)
            ;;        (bottom array en)
            ;;        (left array en)
            ;;        (right array en))
            ;;     0)
            (if (plusp other-diagonals)
                (+ (top-left array 1qn)
                   (top-right array 1qn)
                   (bottom-left array 1qn)
                   (bottom-right array 1qn))
                0)))))))

;; 2023 21 2: '131453890353208129'
;; That's not the right answer.
;; (Cached until 2023-12-21 15:55:22)
;; 131453890353208129

(defun same-skipped (n)
  (cond ((< n 3) 0)
        ((oddp n) (1- (* n n)))
        ((evenp n) (1- (* (1- n) (1- n))))))

(defun other-skipped (n)
  (cond ((< n 2) 0)
        ((evenp n) (* n n))
        ((oddp n) (* (1- n) (1- n)))))

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
        ((< n 1) 0)
        ((oddp n) n)
        ((evenp n) (1- n))))

(defun other-diagonals (n)
  (cond ((minusp n) (error "neg"))
        ((< n 1) 0)
        ((oddp n) (1- n))
        ((evenp n) n)))

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
  (count-steps array n (list (maxp array) (midp array))))

(defun left (array n)
  (when *trace* (print (list 'left n)))
  (count-steps array n (list (midp array) (maxp array))))

(defun top-left (array n)
  (when *trace* (print (list 'top-left n)))
  (count-steps array n (list (maxp array) (maxp array))))

(defun top-right (array n)
  (when *trace* (print (list 'top-right n)))
  (count-steps array n (list (maxp array) 0)))

(defun bottom (array n)
  (when *trace* (print (list 'bottom n)))
  (count-steps array n (list 0 (midp array))))

(defun right (array n)
  (when *trace* (print (list 'right n)))
  (count-steps array n (list (midp array) 0)))

(defun bottom-right (array n)
  (when *trace* (print (list 'bottom-right n)))
  (count-steps array n (list 0 0)))

(defun bottom-left (array n)
  (when *trace* (print (list 'bottom-left n)))
  (count-steps array n (list 0 (maxp array))))

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

;; 2023 21 2: '601441027160729'
;; That's not the right answer.
;; (Cached until 2023-12-21 18:21:13)
;; 601441027160729

;; 2023 21 2: '601435096990471'
;; That's not the right answer.
;; (Cached until 2023-12-21 23:06:35)
;; 601435096990471


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

(defun tracen2 ()
  (let ((*trace* (make-array '(35 35) :initial-element #\.))
        (array (make-array '(7 7) :initial-element #\.))
        (1qn 3)
        (3qn 10))
    (flet ((offset (r c) (list (* r 7) (* c 7))))
      (let ((*parity-test* #'=))
        (let ((*ofs* (offset 0 2)))
          (top array 7))
        (let ((*ofs* (offset 3 3)))
          (count-steps* array 10 '(3 3)))
        (let ((*ofs* (offset 4 2)))
          (bottom array 7))
        (let ((*ofs* (offset 2 4)))
          (right array 7))
        (let ((*ofs* (offset 2 2)))
          (count-steps array 10 '(3 3)))
        (let ((*ofs* (offset 2 0)))
          (left array 7))
        (let ((*ofs* (offset 1 1)))
          (top-left array 3qn))
        (let ((*ofs* (offset 1 3)))
          (top-right array 3qn))
        (let ((*ofs* (offset 3 1)))
          (bottom-left array 3qn)))
      
      (let ((*parity-test* #'/=))

        (let ((*ofs* (offset 0 1)))
          (top-left array 1qn))
        (let ((*ofs* (offset 1 0)))
          (top-left array 1qn))

        (let ((*ofs* (offset 0 3)))
          (top-right array 1qn))
        (let ((*ofs* (offset 1 4)))
          (top-right array 1qn))

        (let ((*ofs* (offset 4 1)))
          (bottom-left array 1qn))
        (let ((*ofs* (offset 3 0)))
          (bottom-left array 1qn))
        ;; (let ((*ofs* (offset 4 3)))
        ;;   (top-right array 1qn))
        ;; (let ((*ofs* (offset 3 4)))
        ;;   (top-right array 1qn))


        (let ((*ofs* (offset 1 2)))
          (count-steps array 10 '(3 3)))
        ;; (let ((*ofs* (offset 2 1)))
        ;;   (count-steps array 10 '(3 3)))
        (let ((*ofs* (offset 2 3)))
          (count-steps array 10 '(3 3)))
        (let ((*ofs* (offset 3 2)))
          (count-steps array 10 '(3 3)))))
    (terpri)
    (aoc:print-array *trace*)))