;;;; day13.lisp

(in-package :aoc2023.day13)

(defun hash (lines)
  (loop with hash-table = (make-hash-table :test 'equal)
        for l in lines
        for i from 0
        do (push i (gethash l hash-table))
        finally (return hash-table)))

(defun line-of-reflection (hash-table)
  (let ((lookup (reverse-hash hash-table)))
    ;; (terpri)
    ;; (print-hash-table lookup)
    (flet ((reflects (pair)
             (and (rest pair)
                  
;                  (prog1 (print pair) (terpri))
                  (loop for l downfrom (apply #'max pair)
                        for r from (apply #'min pair)
                        for ll = (gethash l lookup)
                        for lr = (gethash r lookup)
                        while (and ll lr)
                        ;; do (format t "~3d ~a~%~3d ~a~%" l ll r lr)
                        always (equal ll lr)))))
      (let ((pair (first (remove-if-not #'reflects (pairs hash-table)))))
        (and pair (apply #'max pair))))))

(defun pairs (hash-table)
  (let (result)
    (dolist (v (a:hash-table-values hash-table) result)
      (when (rest v)
        (a:map-combinations (lambda (x)
                              ;; (when (= 1 (- (apply #'max x) (apply #'min x)))
                              ;;   (push x result)))
                              (push x result))
                            v :length 2)))))

(defun print-hash-table (hash-table)
  (aoc:print-indexed-lines
   (mapcar (lambda (k) (gethash k hash-table))
           (sort (a:hash-table-keys hash-table) #'<))))

(defun reverse-hash (hash)
  (loop with reversed = (make-hash-table)
        for v being the hash-keys of hash
        do (loop for k in (gethash v hash)
                 do (setf (gethash k reversed) v))
        finally (return reversed)))

(defun transpose (lines)
  (apply #'map 'list
         (lambda (&rest columns)
           (coerce columns 'string))
         lines))

(defun score (section)
  (let ((vertical (line-of-reflection (hash (transpose (aoc:lines section)))))
        (horizontal (line-of-reflection (hash (aoc:lines section)))))
    ;; (unless (or vertical horizontal)
    ;;   (format t "~a~%~%" section))
    (+ (or vertical 0) (* 100 (or horizontal 0)))))


;; (defun score (section)
;;   (+ (* 100 (or (line-of-reflection (hash (aoc:lines section))) 0))
;;      (or (line-of-reflection (hash (transpose (aoc:lines section)))) 0)))

(defun part1 (input)
  (reduce #'+ (mapcar #'score (aoc:sections input))))

(defun smudge? (a b)
  (= 1 (loop for x across a
             for y across b
             count (not (char= x y)))))

(defun line-of-reflection* (hash-table)
  (let ((lookup (reverse-hash hash-table)))
    ;; (terpri)
    ;; (print-hash-table lookup)
    (flet ((reflects (pair)
             (and (rest pair)
                  
;                  (prog1 (print pair) (terpri))
                  (loop for l downfrom (apply #'min pair)
                        for r from (apply #'max pair)
                        for ll = (gethash l lookup)
                        for lr = (gethash r lookup)
                        for count from 0
                        while (and ll lr)
;                        do (format t "~3d:~%~3d ~a~%~3d ~a~%" count l ll r lr)
                        count (equal ll lr) into equals
                        count (smudge? ll lr) into smudges
                        finally  (return (and (eql (1- count) equals)
                                              (eql 1 smudges)
                                              pair))))))
      (let ((pair (first (remove-if-not #'reflects (smudge-pairs lookup)))))
        (and pair (apply #'max pair))))))

(defun smudge-pairs (lookup)
  (let (result)
    (a:map-combinations
     (lambda (x)
       (destructuring-bind ((ai a) (bi b)) x
         (when (and (= 1 (- (max ai bi) (min ai bi)))
                    (or (equal a b)
                        (smudge? a b)))
           (push (list ai bi) result))))
     (mapcar (lambda (i) (list i (gethash i lookup)))
             (a:hash-table-keys lookup))
     :length 2)
    result))


(defun score* (section)
  (let ((vertical (line-of-reflection* (hash (transpose (aoc:lines section)))))
        (horizontal (line-of-reflection* (hash (aoc:lines section)))))
    ;; (unless (or vertical horizontal)
    ;;   (format t "~a~%~%" section))
    (+ (or vertical 0) (* 100 (or horizontal 0)))))


(defun part2 (input)
  (reduce #'+ (mapcar #'score* (aoc:sections input))))

;; 2023 13 2: '14516'
;; That's not the right answer; your answer is too low.
;; Please wait one minute before trying again.
;; (Cached until 2023-12-13 08:15:16)

;; 2023 13 2: '31974'
;; That's the right answer!
;; (Cached until 2023-12-13 08:30:04)

(defparameter *test*
  "#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#")

(defparameter *test2*
"##.####.######.##
.#.#..#.#....#.#.
...#..#...##...#.
###....########..
#..#..#..####..#.
.#.#..#.#.##.#.#.
...####...##...##
..##..##......##.
##.#..#.######.#.
..#....#....#.#..
..#....#......#..
#...##...####...#
#.######.####.###
..##..##......##.
#........####....
")