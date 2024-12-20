;;;; day20.lisp

(in-package :aoc2024.day20)

(defun find-pos (map c)
  (loop for row below (array-dimension map 0)
        do (loop for col below (array-dimension map 1)
                 when (equal c (aref map row col))
                   do (return-from find-pos (list col row)))))

(defun donep (map goal)
  (when goal
    (destructuring-bind (x y) (find-pos map goal)
      (lambda (n)
        (and (equal x (first n))
             (equal y (second n)))))))

(defun neighbours (map p)
  (destructuring-bind (x y) p
    (loop for (dx dy) in '((1 0) (0 1) (-1 0) (0 -1))
          for nx = (+ x dx)
          for ny = (+ y dy)
          when (walkable? map nx ny)
            collect (list nx ny))))

(defun walkable? (map x y)
  (find (and (and (array-in-bounds-p map y x))
             (aref map y x))
        ".SE"))

(defun start (map where)
  (if (listp where)
      where
      (find-pos map where)))

(defun pos (x)
  (if (listp x)
      (subseq x 0 2)
      (pos (dijkstra:item x))))

(defun search* (map start end)
  (let ((result (dijkstra:search* (start map start) (a:curry #'neighbours map) :donep (donep map end))))
    (if (listp result)
        result
        (dijkstra:distance result))))

(defun cheat-from (map d p)
  (destructuring-bind (x y) p
    (loop for dx from (- d) to d
          nconc (loop for dy from (- d) to d
                      for nx = (+ x dx)
                      for ny = (+ y dy)
                      when (and (<= (distance x y nx ny) d)
                                (walkable? map nx ny)
                                (not (and (eql x nx) (eql y ny))))
                        collect (list nx ny)))))

(defun distance (x1 y1 x2 y2)
  (+ (abs (- x1 x2))
     (abs (- y1 y2))))

(defun count-cheats (map d goodness)
  (let* ((cheats (make-hash-table :test 'equal))
         (uncheated-distance (search* map #\S #\E))
         (from-start (distances map #\S))
         (to-end (distances map #\E)))
    (dolist (cheat-start (a:hash-table-keys from-start))
      (dolist (cheat-end (cheat-from map d cheat-start))
        (a:when-let (to-end (gethash cheat-end to-end))
          (let* ((distance (+ (gethash cheat-start from-start) to-end))
                 (cheat-distance (apply #'distance (append cheat-start cheat-end)))
                 (cheated (- uncheated-distance distance cheat-distance)))
            (when (<= goodness cheated)
              (incf (gethash cheated cheats 0)))))))
    (values (reduce #'+ (a:hash-table-values cheats)) cheats)))

(defun distances (map start)
  (let ((distances (make-hash-table :test 'equal)))
    (dolist (node (search* map start nil))
      (setf (gethash (subseq (dijkstra:item node) 0 2) distances)
            (dijkstra:distance node)))
    distances))

(defun part1 (input)
  (count-cheats (aoc:to-array input) 2 100))

(defun part2 (input)
  (count-cheats (aoc:to-array input) 20 100))
