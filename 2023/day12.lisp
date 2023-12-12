;;;; day12.lisp

(in-package :aoc2023.day12)

(defparameter *test*
"???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1")

(defun parse-line (line)
  (destructuring-bind (pattern counts) (str:split " " line)
    (list pattern (mapcar #'parse-integer (str:split "," counts)))))

(defun parse (input)
  (mapcar #'parse-line (aoc:lines input)))

;; (defun arrangements (position pattern counts)
;;   (cond ((null counts) (if (loop for i from position below (length pattern)
;;                                  never (char= #\# (aref pattern i)))
;;                            (prog1 1 position)
;;                            nil))
;;         ((not (space-left? position pattern counts)) nil)
;;         ((<= (length pattern) position) nil)
;;         ((char= #\# (aref pattern position))
;;          (and (match-at? position pattern (first counts))
;;               (arrangements (+ (first counts) position) pattern (rest counts))))
;;         ((match-at? position pattern (first counts))
;;          (let ((here (arrangements (+ 1 (first counts) position) pattern (rest counts)))
;;                (cont (arrangements (1+ position) pattern counts)))
;;            (if here
;;                (+ here (or cont 0))
;;                cont)))
;;         ((find (aref pattern position) "?.")
;;          (arrangements (1+ position) pattern counts))
;;         (t nil)))

;; (defun space-left? (position pattern counts)
;;   (<= (+ (1- (length counts)) (reduce #'+ counts))
;;       (- (length pattern) position)))

;; (defun match-at? (position pattern count)
;;   (flet ((broken? (i) (find (aref pattern i) "#?"))
;;          (end? (i) (or (not (array-in-bounds-p pattern i))
;;                        (find (aref pattern i) ".?"))))
;;     (and (end? (1- position))
;;          (loop for i from position
;;                repeat count
;;                always (broken? i))
;;          (end? (+ position count)))))

;; (defun combine (sum rest)
;;   (if (null rest)
;;       0
;;       (+ sum rest)))

;; (defun part1 (input)
;;   (loop for (pattern counts) in (mapcar #'parse (aoc:lines input))
;;         ;do (print (list pattern counts '=> (arrangements 0 pattern counts)))
;;         sum (arrangements 0 pattern counts)))

;; ;;; 8312 too high
;; ;;; 2679 too low

;; (defun arrangements (counts length)
;;   (cond ((null counts) (list (make-string length :initial-element #\.)))
;;         ((< 0 length) nil)
;;         (t (append (mapcar (lambda (s) (format nil ".~a" s))
;;                            (arrangements counts (1- length)))
;;                    (mapcar (lambda (s) (format nil "~a~a"
;;                                                (make-string (first counts) :initial-element #\#)
;;                                                (arrangements ())
;;                                                )))))))

;; (defun spaces (n total)
;;   (if (zerop n)
;;       nil
;;       (loop for s from 1 to (- total (1- n))
;;             collect (cons s (spaces (1- n) (- total s))))))

(defun arrangements (counts length)
  (labels ((str (c l) (make-string l :initial-element c))
           (broken () (str #\# (first counts)))
           (spacer () (if (rest counts) "." ""))
           (required-length () (+ (max 0 (1- (length (rest counts)))) (reduce #'+ counts) (length (spacer)))))
    (cond ((null counts) (list (str #\. length)))
          ((> (required-length) length) nil)
          (t (let ((prefix (str:concat (broken) (spacer))))
               (append (mapcar (a:curry #'str:concat prefix) (arrangements (rest counts) (- length (length prefix))))
                       (mapcar (a:curry #'str:concat ".") (arrangements counts (1- length)))))))))

(defun count-valid-arrangements (pattern counts)
  (count-if (pattern-predicate pattern) (arrangements counts (length pattern))))

(defun pattern-predicate (pattern)
  (lambda (s)
    (every (lambda (p x) (or (char= #\? p) (char= p x))) pattern s)))

(defun part1 (input)
  (reduce #'+ (mapcar (a:curry #'apply #'count-valid-arrangements) (parse input))))

(defun unfold (records)
  (flet ((five (x) (loop repeat 5 collect x)))
    (mapcar (lambda (record)
              (list (format nil "~{~a~^?~}" (five (first record)))
                    (apply #'append (five (second record)))))
            records)))

(defun arrangements2 (pattern counts)
  (let ((trace (make-string (length pattern) :initial-element #\.)))
    (labels ((reset-trace () (setf trace (make-string (length pattern) :initial-element #\.)))
             (p (i) (and (array-in-bounds-p pattern i) (aref pattern i)))
             (empty? (i) (find (p i) ".?"))
             (broken? (i) (find (p i) "#?"))
             (joker? (i) (char= #\? (p i)))
             (empty-end? (start) (loop for i from start below (length pattern)
                                       always (empty? i)))
             (match? (start count)
               ;; (when (= start 2) (break))
               (and (loop for i from start
                          repeat count
                          always (broken? i))
                    (or (empty? (+ start count))
                        (null (p (+ start count))))))
             (rec (i counts sum cont)
               (cond 
                 ;; ((not (array-in-bounds-p pattern i))
                 ;;  (funcall cont (+ sum (if counts 0 1))))
                 ((null counts)
                  (funcall cont (if (empty-end? i)
                                    (progn ;(print trace)
                                      (1+ sum)
                                      )
                                    sum)))
                 ((match? i (first counts))
                  (loop for ix from i repeat (first counts)
                        do (setf (aref trace ix) #\#))
                  (rec (+ i (first counts)
                          (if (rest counts) 1 0))
                       (rest counts)
                       sum
                       (if (joker? i)
                           (lambda (sum) (reset-trace) (rec (1+ i) counts sum cont))
                           cont)))
                 ((empty? i)
                  (rec (1+ i) counts sum cont))
                 (t (reset-trace) (funcall cont sum)))))
      (rec 0 counts 0 #'identity))))

(defun part2 (input)
  (reduce #'+ (mapcar (a:curry #'apply #'count-valid-arrangements) (unfold (parse input)))))

