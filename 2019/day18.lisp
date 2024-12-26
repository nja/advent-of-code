;;;; day18.lisp

(in-package :aoc2019.day18)

(defparameter *test*
"#######
#a.#Cd#
##...##
##.@.##
##...##
#cB#Ab#
#######"
)

(defun parse (input)
  (aoc:to-array input))

(defun start-position (array)
  (cons 0 (loop for i below (array-total-size array)
                when (eql #\@ (row-major-aref array i))
                  append (multiple-value-list (truncate i (array-dimension array 1))))))

(defun keys (array)
  (loop with keys = 0
        for i below (array-total-size array)
        for c = (row-major-aref array i)
        when (lower-case-p c)
          do (setf keys (with-key keys c))
        finally (return keys)))

(defun keybit (key)
  (ash 1 (- (char-code (char-downcase key)) (char-code #\a))))

(defun with-key (keys key)
  (logior keys (keybit key)))

(defun has-key? (keys key)
  (plusp (logand keys (keybit key))))

(defun donep (array)
  (let ((keys (keys array)))
    (lambda (node)
      (eql keys (elt node 0)))))

(defparameter *distances* nil)

(defun distance (current neigbour)
  (gethash (dkey (first current) (first neigbour)) *distances*))

(defun dkey (a b)
  (cons (min a b) (max a b)))

(defun possible-keys (array node)
  (loop with keys = (first node)
        for snode in (dijkstra:search* node (a:curry #'neighbours array))
        for (nkeys row col) = (dijkstra:item snode)
        for distance = (dijkstra:distance snode)
        for x = (aref array row col)
        when (and (lower-case-p x) (not (has-key? keys x)))
          do (setf (gethash (dkey nkeys keys) *distances*) distance)
          and collect (list keys row col)))

(defun neighbours (array node)
  (loop with (keys row col) = node
        for (dr dc) in '((1 0) (-1 0) (0 1) (0 -1))
        for nr = (+ row dr)
        for nc = (+ col dc)
        for x = (aref array nr nc)
        if (or (find x ".@")
               (and (upper-case-p x) (has-key? keys x)))
          collect (list keys nr nc)
        if (lower-case-p x)
          collect (list (with-key keys x) nr nc)))

(defun sref (array row col)
  (and (array-in-bounds-p array row col)
       (aref array row col)))

(defun part1 (input)
  (let ((array (aoc:to-array input))
        (*distances* (make-hash-table :test 'equal)))
    (dijkstra:distance (dijkstra:search* (start-position array)
                                         (a:curry #'possible-keys array)
                                         :donep (donep array)
                                         :distancef #'distance))))

(defun update (array)
  (destructuring-bind (row col) (rest (start-position array))
    (loop for (dr dc) in '((0 0) (1 0) (-1 0) (0 1) (0 -1))
          do (setf (aref array (+ row dr) (+ col dc)) #\#))
    (loop for (dr dc) in '((1 1) (-1 -1) (-1 1) (1 -1))
          do (setf (aref array (+ row dr) (+ col dc)) #\@))    
    array))

(defun neighbours2 (array node)
  (loop with keys = (aref node 0)
        for i from 1 by 2 below (length node)
        for row = (aref node i)
        for col = (aref node (1+ i))
        unless (by-door? array keys row col)
          nconc (loop for (nkeys nr nc) in (neighbours array (list keys row col))
                      collect (node2 node i nkeys nr nc))))

(defun node2 (old i keys row col)
  (let ((node (copy-seq old)))
    (setf (aref node 0) keys
          (aref node i) row
          (aref node (1+ i)) col)
    node))

(defun by-door? (array keys row col)
  (loop for (dr dc) in '((1 0) (-1 0) (0 1) (0 -1))
        for x = (sref array (+ row dr) (+ col dc))
        thereis (and (upper-case-p x) (not (has-key? keys x)))))

(defun part2 (input)
  (let ((array (update (aoc:to-array input))))
    (dijkstra:distance (dijkstra:search* (coerce (start-position array) 'vector)
                                         (a:curry #'neighbours2 array)
                                         :donep (donep array)))))