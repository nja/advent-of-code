;;;; day16.lisp

(in-package :aoc2024.day16)

(defun neighbours (map n)
  (destructuring-bind (x y dx dy) n
    (let (results)
      (labels ((walkable? (x y)
                 (find (and (array-in-bounds-p map y x) (aref map y x)) ".ES"))
               (collect (x y dx dy)
                 (push (list x y dx dy) results)))
        (when (walkable? (+ x dx) (+ y dy))
          (collect (+ x dx) (+ y dy) dx dy))
        (let ((rdx dy)
              (rdy (- dx))
              (ldx (- dy))
              (ldy dx))
          (when (walkable? (+ x rdx) (+ y rdy))
            (collect x y rdx rdy))
          (when (walkable? (+ x ldx) (+ y ldy))
            (collect x y ldx ldy))))
      results)))

(defun distance (a b)
  (if (and (eql (first a) (first b)) (eql (second a) (second b)))
      1000
      1))

(defun find-pos (map c)
  (loop for row below (array-dimension map 0)
        do (loop for col below (array-dimension map 1)
                 when (equal c (aref map row col))
                   do (return-from find-pos (list col row)))))

(defun start (map where)
  (if (listp where)
      where
      (append (find-pos map where) '(1 0))))

(defun donep (map goal)
  (destructuring-bind (x y) (find-pos map goal)
    (lambda (n)
      (and (eql x (first n)) (eql y (second n))))))

(defun search* (map from &optional to)
  (dijkstra:search* (start map from) (a:curry #'neighbours map) :donep (when to (donep map to)) :distancef #'distance))

(defun part1 (input)
  (dijkstra:distance (search* (aoc:to-array input) #\S #\E)))

(defun ends (map)
  (let ((nodes (search* map #\S)))
    (mapcar (a:compose #'reverse-node #'dijkstra:item)
            (remove-if-not (let ((donep (donep map #\E)))
                             (lambda (n)
                               (funcall donep (dijkstra:item n))))
                           nodes))))

(defun distances (map)
  (let ((distances (make-hash-table :test 'equal)))
    (dolist (end (ends map))
      (dolist (node (search* map end))
        (setf (gethash (reverse-node (dijkstra:item node)) distances)
              (dijkstra:distance node))))
    distances))

(defun reverse-node (node)
  (append (butlast node 2) (mapcar (a:curry #'* -1) (last node 2))))

(defun count-tiles-on-optimal-paths (map)
  (let ((tiles (make-hash-table :test 'equal))
        (distances (distances map))
        (optimal (dijkstra:distance (search* map #\S #\E))))
    (flet ((add (n) (setf (gethash (subseq n 0 2) tiles) t)))
      (dolist (node (search* map #\S))
        (let* ((n (dijkstra:item node))
               (to-end (gethash n distances)))
          (when (and to-end (= optimal (+ (dijkstra:distance node) to-end)))
            (add n)))))
    (hash-table-count tiles)))

(defun part2 (input)
  (count-tiles-on-optimal-paths (aoc:to-array input)))
