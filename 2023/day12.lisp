;;;; day12.lisp

(in-package :aoc2023.day12)

(defparameter *test*
"???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1")

(defun parse (line)
  (destructuring-bind (pattern counts) (str:split " " line)
    (list pattern (mapcar #'parse-integer (str:split "," counts)))))

(defun arrangements (position pattern counts)
  (cond ((null counts) (if (loop for i from position below (length pattern)
                                 never (char= #\# (aref pattern i)))
                           (prog1 1 position)
                           nil))
        ((not (space-left? position pattern counts)) nil)
        ((<= (length pattern) position) nil)
        ((char= #\# (aref pattern position))
         (and (match-at? position pattern (first counts))
              (arrangements (+ (first counts) position) pattern (rest counts))))
        ((match-at? position pattern (first counts))
         (+ (or (arrangements (+ (first counts) position) pattern (rest counts)) 0)
            (or (arrangements (1+ position) pattern counts) 0)))
        ((find (aref pattern position) "?.")
         (arrangements (1+ position) pattern counts))
        (t nil)))

(defun space-left? (position pattern counts)
  (<= (+ (1- (length counts)) (reduce #'+ counts))
      (- (length pattern) position)))

(defun match-at? (position pattern count)
  (flet ((broken? (i) (find (aref pattern i) "#?"))
         (end? (i) (or (not (array-in-bounds-p pattern i))
                       (find (aref pattern i) ".?"))))
    (and (end? (1- position))
         (loop for i from position
               repeat count
               always (broken? i))
         (end? (+ position count)))))

(defun combine (sum rest)
  (if (null rest)
      0
      (+ sum rest)))

(defun part1 (input)
  (loop for (pattern counts) in (mapcar #'parse (aoc:lines input))
        do (print (list pattern counts '=> (arrangements 0 pattern counts)))))