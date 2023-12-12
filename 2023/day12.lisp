;;;; day12.lisp

(in-package :aoc2023.day12)

(defparameter *test*
"#.#.### 1,1,3
.#...#....###. 1,1,3
.#.###.#.###### 1,3,1,6
####.#...#... 4,1,1
#....######..#####. 1,6,5
.###.##....# 3,2,1")

(defun parse (line)
  (destructuring-bind (pattern counts) (str:split " " line)
    (list pattern (mapcar #'parse-integer (str:split "," counts)))))

(defun arrangements (position pattern counts)
  (cond ((null counts) 1)
        ((not (space-left? position pattern counts)) nil)
        ((= position (length pattern)) nil)
        ((match-at? position pattern (first counts))
         (+ (or (arrangements (1+ position) pattern (rest counts)) 0)
            (arrangements (1+ position) pattern counts)))
        (t (arrangements (1+ position) pattern counts))))

(defun space-left? (position pattern counts)
  (<= (+ (1- (length counts)) (reduce #'+ counts))
      (- (length pattern) position)))

(defun match-at? (position pattern count)
  (flet ((broken? (i) (find (aref pattern position) "#?"))
         (end? (i) (or (not (array-in-bounds-p array i))
                       (char= #\. (aref pattern position)))))
    (and (end? (1- position))
         (loop for i from position
               repeat count
               always (broken? i))
         (end (+ position count)))))

(defun combine (sum rest)
  (if (null rest)
      0
      (+ sum rest)))