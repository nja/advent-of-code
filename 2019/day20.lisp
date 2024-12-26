;;;; day20.lisp

(in-package :aoc2019.day20)

(defun parse (input)
  (aoc:to-array input))

(defun portals (array)
  (loop with portals = (make-hash-table :test 'equal)
        for row below (array-dimension array 0)
        do (loop for col below (array-dimension array 1)
                 for label = (label array row col)
                 when label
                   do (push (list row col) (gethash label portals))
                      (setf (gethash (list row col) portals) label))
        finally (return portals)))

(defun label (array row col)
  (when (eql #\. (aref array row col))
    (loop for (dr dc) in '((1 0) (0 1) (-1 0) (0 -1))
          for r = (+ row dr)
          for c = (+ col dc)
          for x = (and (array-in-bounds-p array r c) (aref array r c))
          when (alphanumericp x)
            return (sort (format nil "~a~a" (aref array r c) (aref array (+ r dr) (+ c dc))) #'char<))))

(defun neighbours (array portals pos)
  (append
   (teleport portals pos)
   (loop for d in '((1 0) (0 1) (-1 0) (0 -1))
         for p = (add pos d)
         when (eql #\. (apply #'aref array p))
           collect p)))

(defun add (a b)
  (mapcar #'+ a b))

(defun teleport (portals pos)
  (a:when-let (label (gethash pos portals))
    (remove pos (gethash label portals) :test 'equal)))

(defun donep (portals)
  (a:curry #'equal (first (gethash "ZZ" portals))))

(defun part1 (input)
  (let* ((array (aoc:to-array input))
         (portals (portals array)))
    (dijkstra:distance (dijkstra:search* (first (gethash "AA" portals))
                                         (a:curry #'neighbours array portals)
                                         :donep (donep portals)))))

(defun outer? (array pos)
  (destructuring-bind (row col) pos
    (or (eql 2 row)
        (eql 2 col)
        (eql (array-dimension array 0) (+ row 3))
        (eql (array-dimension array 1) (+ col 3)))))

(defun neighbours2 (array portals pos)
  (append
   (teleport2 array portals pos)
   (destructuring-bind (level . pos) pos
     (loop for d in '((1 0) (0 1) (-1 0) (0 -1))
           for p = (add pos d)
           when (eql #\. (apply #'aref array p))
             collect (cons level p)))))

(defun teleport2 (array portals pos)
  (destructuring-bind (level . pos) pos
    (let ((outer? (outer? array pos)))
      (unless (and outer? (zerop level))
        (a:when-let (destination (first (teleport portals pos)))
          (list (cons (if outer? (1- level) (1+ level)) destination)))))))

(defun donep2 (portals)
  (a:curry #'equal (cons 0 (first (gethash "ZZ" portals)))))

(defun part2 (input)
  (let* ((array (aoc:to-array input))
         (portals (portals array)))
    (dijkstra:distance (dijkstra:search* (cons 0 (first (gethash "AA" portals)))
                                         (a:curry #'neighbours2 array portals)
                                         :donep (donep2 portals)))))
