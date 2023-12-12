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

(defun unfold (record)
  (flet ((five (x) (loop repeat 5 collect x)))
    (list (format nil "~{~a~^?~}" (five (first record)))
          (apply #'append (five (second record))))))

(defun arrangements2 (pattern counts)
  (let (;(trace (make-string (length pattern) :initial-element #\.))
        )
    (labels (;(reset-trace () (setf trace (make-string (length pattern) :initial-element #\.)))
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
                 ((> (+ i (reduce #'+ counts)
                        (max 0 (1- (length counts))))
                     (length pattern))
                  (funcall cont sum))
                 ((not (array-in-bounds-p pattern i))
                  (funcall cont (+ sum (if counts 0 1))))
                 ((null counts)
                  (funcall cont (if (empty-end? i)
                                    (progn ;(print trace)
                                      (1+ sum)
                                      )
                                    sum)))
                 ((match? i (first counts))
                  ;; (loop for ix from i repeat (first counts)
                  ;;       do (setf (aref trace ix) #\#))
                  (rec (+ i (first counts)
                          (if (rest counts) 1 0))
                       (rest counts)
                       sum
                       (if (joker? i)
                           (lambda (sum); (reset-trace)
                             (rec (1+ i) counts sum cont))
                           cont)))
                 ((empty? i)
                  (rec (1+ i) counts sum cont))
                 (t ;(reset-trace)
                    (funcall cont sum)))))
      (rec 0 counts 0 #'identity))))

(defun arrs2 (pattern counts)
  (let ((counts (coerce counts 'vector)))
    (labels ((rec (pix cix)
               (flet ((fits? ()
                        (let ((c (aref counts cix)))
                          (and (loop for i from pix repeat c
                                     always (and (array-in-bounds-p pattern i)
                                                 (find (aref pattern i) "#?")))
                               (or (not (array-in-bounds-p pattern (+ pix c)))
                                   (find (aref pattern (+ pix c)) ".?")))))
                      (next-pix ()
                        (+ pix (aref counts cix) 1)))
                 (cond ((= (length counts) cix)
                        (if (loop for i from pix below (length pattern)
                                  always (find (aref pattern pix) ".?"))
                            1
                            0))
                       ((<= (length pattern) pix)
                        0)
                       ((case (aref pattern pix)
                          (#\. (rec (1+ pix) cix))
                          (#\# (if (fits?)
                                   (rec (next-pix) (1+ cix))
                                   0))
                          (#\? (+ (if (fits?)
                                      (rec (next-pix) (1+ cix))
                                      0)
                                  (rec (1+ pix) (1+ cix))))))))))
      (rec 0 0))))

(defun part2 (input)
  (reduce #'+ (mapcar (lambda (x)
                        (let ((res (apply #'binary-arrangements (unfold x))))
                          (print (list x res))
                          res))
                      (parse input))))



(defun ppart2 (input)
  (let ((cache (get-cache))
        (lock (bt:make-lock)))
    (reduce #'+ (lparallel:pmap 'list (lambda (x)
                                        (let* ((c (gethash x cache))
                                               (res (or c (apply #'binary-arrangements (unfold x)))))
                                          (unless c
                                            (bt:with-lock-held (lock)
                                              (append-cache (list x res))
                                              (print (list x res))))
                                          res))
                                (parse input)))))

;; (defun ppart2 (input)
;;   (reduce #'+ (lparallel:pmap 'list (lambda (x)
;;                                 (print x)
;;                                 (apply #'arrangements2 x)) (unfold (parse input)))))


(defun binary-arrangements (pattern counts)
  (cond ((null counts)
         (if (every (lambda (c) (find c "?.")) pattern)
             1
             0))
        ((zerop (length pattern)) 0)
        (t (let ((max (reduce #'max counts)))
             (let* ((max-ix (position max counts))
                    (left (subseq counts 0 max-ix))
                    (right (subseq counts (1+ max-ix))))
               (loop for (l r) in (splits pattern max)
                     sum (if (< (length l) (length r))
                             (let ((lres (binary-arrangements l left)))
                               (if (zerop lres)
                                   0
                                   (* lres (binary-arrangements r right))))
                             (let ((rres (binary-arrangements r right)))
                               (if (zerop rres)
                                   0
                                   (* rres (binary-arrangements l left)))))))))))


;; (defun binary-arrangements (pattern counts)
;;   (if (null counts)
;;       (if (every (lambda (c) (find c "?.")) pattern)
;;           1
;;           0)
;;       (let ((max (reduce #'max counts)))
;;         (if (< 2 max)
;;             (arrangements2 pattern counts)
;;             (let* ((max-ix (position max counts))
;;                    (left (subseq counts 0 max-ix))
;;                    (right (subseq counts (1+ max-ix))))
;;               (loop for (l r) in (splits pattern max)
;;                     sum (* (binary-arrangements l left)
;;                            (binary-arrangements r right))))))))

(defun splits (pattern count)
  (flet ((end? (i) (or (not (array-in-bounds-p pattern i))
                       (find (aref pattern i) "?."))))
    (loop for i from 0 upto (- (length pattern) count)
          when (and (end? (1- i))
                    (loop for x from i repeat count
                          always (find (aref pattern x) "#?"))
                    (end? (+ i count)))
            collect (list (subseq pattern 0 (max 0 (1- i)))
                          (subseq pattern (min (length pattern) (+ i count 1)))))))

;;; 525152

(defun get-cache ()
  (let ((hash-table (make-hash-table :test 'equalp)))
    (dolist (l (aoc:lines (a:read-file-into-string "c:/Users/johan/quicklisp/local-projects/advent-of-code/2023/day12-cache.txt")) hash-table)
      (destructuring-bind (k v) (read-from-string l)
        (setf (gethash k hash-table) v)))))

(defun append-cache (line)
  (with-open-file (out "c:/Users/johan/quicklisp/local-projects/advent-of-code/2023/day12-cache.txt"
                       :direction :output :if-exists :append)
    (print line out)))