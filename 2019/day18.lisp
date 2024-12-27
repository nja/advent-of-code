;;;; day18.lisp

(in-package :aoc2019.day18)

(defun parse (input)
  (aoc:to-array input))

(defun start-position (array)
  (loop for i below (array-total-size array)
        when (eql #\@ (row-major-aref array i))
          return (multiple-value-list (truncate i (array-dimension array 1)))))

(defun neighbours (array node)
  (loop with (row col) = node
        for (dr dc) in '((1 0) (-1 0) (0 1) (0 -1))
        for nr = (+ row dr)
        for nc = (+ col dc)
        for x = (aref array nr nc)
        unless (eql x #\#)
          collect (list nr nc)))

(defun distance (distances)
  (lambda (a b)
    (let ((a (second a))
          (b (second b)))
      (loop for (k d req) in (gethash a distances)
            when (eq k b)
              return d))))

(defun manhattan (a b)
  (reduce #'+ (mapcar (a:compose #'abs #'-) a b)))

(defun key (a b)
  (cons (min a b) (max a b)))

(defun gather-distances (array)
  (loop with distances = (make-hash-table)
        with starts = 0
        for row below (array-dimension array 0)
        do (loop for col below (array-dimension array 1)
                 for x = (aref array row col)
                 for key = (cond ((eql x #\@) (decf starts))
                                 ((lower-case-p x) (keybit x)))
                 when key
                   do (setf (gethash key distances)
                            (key-distances array (list row col))))
        finally (return distances)))

(defun key-distances (array pos)
  (let ((search (dijkstra:search* pos (a:curry #'neighbours array))))
    (loop for snode in search
          for p = (dijkstra:item snode)
          for x = (apply #'aref array p)
          when (lower-case-p x)
            collect (list (keybit x) (dijkstra:distance snode) (required-keys array snode)))))

(defun required-keys (array snode)
  (loop with keys = 0
        for distance = (dijkstra:distance snode)
        for (row col) = (dijkstra:item snode)
        for x = (aref array row col)
        when (upper-case-p x)
          do (setf keys (with-key keys x))
        do (setf snode (dijkstra:previous snode))
        while snode
        finally (return keys)))

(defun possible-keys (distances node)
  (loop with (keys key) = node
        for (k d req) in (gethash key distances)
        when (and (not (eq key k))
                  (not (has-key? keys k))
                  (has-keys? keys req))
          collect (list (with-key keys k) k)))

(defun test (array)
  (key-distances array (start-position array)))

(defun keys (array)
  (loop with keys = 0
        for i below (array-total-size array)
        for c = (row-major-aref array i)
        when (lower-case-p c)
          do (setf keys (with-key keys c))
        finally (return keys)))

(defun keybit (key)
  (if (integerp key)
      key
      (ash 1 (- (char-code (char-downcase key)) (char-code #\a)))))

(defun with-key (keys key)
  (logior keys (keybit key)))

(defun has-key? (keys key)
  (plusp (logand keys (keybit key))))

(defun has-keys? (keys required-keys)
  (eql required-keys (logand keys required-keys)))

(defun donep (array)
  (let ((keys (keys array)))
    (lambda (node)
      (eql keys (elt node 0)))))

(defun part1 (input)
  (let* ((array (aoc:to-array input))
         (distances (gather-distances array)))
    (dijkstra:distance (dijkstra:search* '(0 -1)
                                         (a:curry #'possible-keys distances)
                                         :donep (donep array)
                                         :distancef (distance distances)))))

(defun update (array)
  (destructuring-bind (row col) (start-position array)
    (loop for (dr dc) in '((0 0) (1 0) (-1 0) (0 1) (0 -1))
          do (setf (aref array (+ row dr) (+ col dc)) #\#))
    (loop for (dr dc) in '((1 1) (-1 -1) (-1 1) (1 -1))
          do (setf (aref array (+ row dr) (+ col dc)) #\@))    
    array))

(defun neighbours2 (distances node)
  (loop with keys = (aref node 0)
        for i from 1 below (length node)
        for key = (aref node i)
        nconc (loop for (k d req) in (gethash key distances)
                    when (and (not (eq key k))
                              (not (has-key? keys k))
                              (has-keys? keys req))
                      collect (node2 node i k))))

(defun node2 (old i k)
  (let ((node (copy-seq old)))
    (setf (aref node 0) (with-key (aref old 0) k)
          (aref node i) k)
    node))

(defun distance2 (distances)
  (lambda (a b)
    (loop for i from 1
          for x = (aref a i)
          for y = (aref b i)
          unless (eql x y)
            return (loop for (k d req) in (gethash x distances)
                         when (eql k y)
                           return d))))

(defun part2 (input)
  (let* ((array (update (aoc:to-array input)))
         (distances (gather-distances array)))
    (dijkstra:distance (dijkstra:search* (vector 0 -1 -2 -3 -4)
                                         (a:curry #'neighbours2 distances)
                                         :donep (donep array)
                                         :distancef (distance2 distances)))))
