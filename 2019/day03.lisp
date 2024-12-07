;;;; day03.lisp

(in-package :aoc2019.day03)

(defun parse (input)
  (mapcar (lambda (line)
            (mapcar (lambda (x)
                      (mapcar #'read-from-string
                              (list (subseq x 0 1) (subseq x 1))))
                    (str:split "," line)))
          (aoc:lines input)))

(defun d (x)
  (getf '(U (0 -1) D (0 1) R (1 0) L (-1 0)) x))

(defun add (a b)
  (mapcar #'+ a b))

(defun map-positions (f directions)
  (loop with pos = '(0 0)
        with distance = 0
        for (dir count) in directions
        for d = (d dir)
        do (loop repeat count
                 do (setf pos (add pos d))
                    (funcall f pos (incf distance)))))

(defun record (directions)
  (let ((record (make-hash-table :test 'equal)))
    (map-positions (lambda (pos distance)
                     (setf (gethash pos record)
                           (min distance (gethash pos record distance))))
                   directions)
    record))

(defun intersections (record directions)
  (let (result)
    (map-positions (lambda (pos d1)
                     (a:when-let (d2 (gethash pos record))
                       (push (cons (+ d1 d2) pos) result)))
                   directions)
    result))

(defun distance (pos)
  (reduce #'+ (mapcar #'abs pos)))

(defun part1 (input)
  (destructuring-bind (a b) (parse input)
    (reduce #'min (intersections (record a) b) :key (a:compose #'distance #'cdr))))

(defun part2 (input)
  (destructuring-bind (a b) (parse input)
    (reduce #'min (intersections (record a) b) :key #'car)))
