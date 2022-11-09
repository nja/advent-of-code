;;;; day20.lisp

(in-package #:aoc2018.day20)

(defun parse (input)
  (with-input-from-string (s input)
    (labels ((peek () (peek-char nil s nil))
             (read* () (read-char s))
             (must-read (x) (assert (equal x (peek))) (read*)))
      (labels ((read-seq ()
                 (let ((seq (make-array 0 :adjustable t :fill-pointer 0)))
                   (flet ((add (x) (vector-push-extend x seq)))
                     (loop (ecase (peek)
                             ((#\N #\S #\E #\W) (add (read*)))
                             (#\( (add (read-options)))
                             ((#\| #\) #\$) (return seq)))))))
               (read-options ()
                 (let (options)
                   (must-read #\()
                   (loop do (push (read-seq) options)
                         while (equal #\| (peek))
                         do (read*))
                   (must-read #\))
                   (nreverse options))))
        (must-read #\^)
        (prog1 (read-seq)
          (must-read #\$))))))

(defun process-elem (positions x)
  (typecase x
    (character (move x positions))
    (list (delete-duplicates (mapcan (a:curry #'process-elem positions) x) :test 'equalp))
    (vector (process-seq positions x))))

(defun process-seq (positions seq)
  (reduce (lambda (p e) (process-elem p e)) seq :initial-value positions))

(defstruct (pos :conc-name (:constructor pos (x y))) x y)

(defun add (&rest positions)
  (reduce (lambda (p1 p2) (pos (+ (x p1) (x p2)) (+ (y p1) (y p2))))
          positions
          :initial-value (pos 0 0)))

(defun move (char positions)
  (mapcar (lambda (p) (add (dir char) (record (add (dir char) p)))) positions))

(defun dir (char)
  (case char
    (#\N (pos 0 -1))
    (#\E (pos 1 0))
    (#\S (pos 0 1))
    (#\W (pos -1 0))))

(defparameter *doors* (make-hash-table :test 'equalp))

(defun record (p)
  (setf (gethash p *doors*) t)
  p)

(defun limits (hash-table)
  (loop for k in (a:hash-table-keys hash-table)
        minimize (- (x k) (if (evenp (y k)) 2 1)) into x-min
        maximize (+ (x k) (if (evenp (y k)) 2 1)) into x-max
        minimize (- (y k) (if (evenp (x k)) 2 1)) into y-min
        maximize (+ (y k) (if (evenp (x k)) 2 1)) into y-max
        finally (return (list x-min y-min x-max y-max))))

(defun print-doors (hash-table)
  (flet ((near? (p) (some (lambda (d) (gethash (add (dir d) p) hash-table)) "NSEW")))
      (aoc:print-indexed-lines
         (loop with (x-min y-min x-max y-max) = (limits hash-table)
               for row from (min -1 y-min) upto (max 1 y-max)
               collect
               (with-output-to-string (s)
                 (loop for col from (min -1 x-min) upto (max 1 x-max)
                       for c = (cond ((= 0 row col) #\X)
                                     ((gethash (pos col row) hash-table)
                                      (if (evenp row) #\| #\-))
                                     ((oddp row) #\#)
                                     ((oddp col) #\#)
                                     (t (if (near? (pos col row)) #\. #\#)))
                       do (princ c s)))))))

(defun neighbours (hash-table pos)
  (flet ((door? (d) (gethash (add (dir d) pos) hash-table))
         (walk (d) (add (dir d) (dir d) pos)))
    (loop for d across "NSEW" when (door? d) collect (walk d))))

(defun find-paths (hash-table)
  (nth-value 1 (d:search* (pos 0 0) (a:curry #'neighbours hash-table) :pathsp t)))

(defun max-doors (search-nodes)
  (reduce #'max (mapcar #'d:distance search-nodes)))

(defun part1 (input)
  (let ((*doors* (make-hash-table :test 'equalp)))
    (process-seq (list (pos 0 0)) (parse input))
    (max-doors (find-paths *doors*))))

(defun part2 (input)
  (let ((*doors* (make-hash-table :test 'equalp)))
    (process-seq (list (pos 0 0)) (parse input))
    (count-if (lambda (x) (< 1000 (d:distance x))) (find-paths *doors*))))
