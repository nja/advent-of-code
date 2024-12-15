;;;; day15.lisp

(in-package :aoc2024.day15)

(defun parse (input &key wide)
  (destructuring-bind (map moves) (aoc:sections input)
    (values (aoc:to-array (if wide (widen map) map)) (remove #\Newline moves))))

(defun swap (map src m)
  (let* ((d (direction m))
         (dst (add src d)))
    (flet ((yes () (rotatef (apply #'aref map src) (apply #'aref map dst)) t))
      (case (apply #'aref map dst)
        (#\. (yes))
        (#\O (and (swap map dst m) (yes)))
        (#\[ (case m
               ((#\< #\>) (and (swap map dst m) (yes)))
               ((#\^ #\v) (and (swap? map (add dst (direction #\>)) m)
                               (swap map dst m)
                               (swap map (add dst (direction #\>)) m)
                               (yes)))))
        (#\] (case m
               ((#\< #\>) (and (swap map dst m) (yes)))
               ((#\^ #\v) (and (swap? map (add dst (direction #\<)) m)
                               (swap map dst m)
                               (swap map (add dst (direction #\<)) m)
                               (yes)))))))))

(defun swap? (map src m)
  (let* ((d (direction m))
         (dst (add src d)))
    (case (apply #'aref map src)
      (#\. t)
      (#\[ (and (swap? map dst m)
                (swap? map (add dst '(0 1)) m)))
      (#\] (swap? map (add src '(0 -1)) m)))))

(defun add (a b)
  (mapcar #'+ a b))

(defun start (map)
  (loop for row below (array-dimension map 0)
        do (loop for col below (array-dimension map 1)
                 when (eql #\@ (aref map row col))
                   do (return-from start (list row col)))))

(defun direction (c)
  (getf '(#\^ (-1  0)
          #\v ( 1  0)
          #\< ( 0 -1)
          #\> ( 0  1))
        c))

(defun moves (map moves)
  (loop with p = (start map)
        for m across moves
        when (swap map p m)
          do (setf p (add p (direction m)))
        finally (return map)))

(defun sum-coordinates (map)
  (loop for row below (array-dimension map 0)
        sum (loop for col below (array-dimension map 1)
                  when (find (aref map row col) "O[")
                    sum (+ (* row 100) col))))

(defun part1 (input)
  (sum-coordinates (multiple-value-call #'moves (parse input))))

(defun widen (string)
  (format nil "~{~a~}" (map 'list (lambda (c) (case c (#\. "..") (#\# "##") (#\O "[]") (#\@ "@.") (t c))) string)))

(defun part2 (input)
  (sum-coordinates (multiple-value-call #'moves (parse input :wide t))))
